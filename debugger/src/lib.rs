// pest. The Elegant Parser
// Copyright (c) 2018-2022 Dragoș Tiselice, Tomas Tauber
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
//! # pest debugger
//!
//! This crate contains definitions for the debugger.
//! A sample CLI-based debugger is available in `main.rs`.
//! Other debugger frontends can be implemented using this
//! crate's `DebuggerContext`:
//!
//! ```
//! use pest_debugger::DebuggerContext;
//! use std::sync::mpsc::sync_channel;
//! let mut context = DebuggerContext::default();
//!
//! context
//! .load_grammar_direct(
//!     "testgrammar",
//!     r#"alpha = { 'a'..'z' | 'A'..'Z' }
//! digit = { '0'..'9' }
//!
//! ident = { !digit ~ (alpha | digit)+ }
//!
//! ident_list = _{ ident ~ (" " ~ ident)* }"#,
//! ).expect("Error: failed to load grammar");
//! context.load_input_direct("test test2".to_owned());
//!
//! let (sender, receiver) = sync_channel(1);
//!
//! context.add_breakpoint("ident".to_owned());
//! for b in context.list_breakpoints().iter() {
//!     println!("Breakpoint: {}", b);
//! }
//! context
//! .run("ident_list", sender)
//! .expect("Error: failed to run rule");
//!
//! let event = receiver.recv().expect("Error: failed to receive event");
//! println!("Received a debugger event: {:?}", event);
//!
//! context.cont().expect("Error: failed to continue");
//!
//! let event = receiver.recv().expect("Error: failed to receive event");
//! println!("Received a debugger event: {:?}", event);
//! ```
//! ## Current Limitations
//! - relies on OS threads instead of stack-full generators
//! - only shows position from the `ParserState` when it reaches a breakpoint
//! - no way to run another rule from a breakpoint, only from the start
#![doc(
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]
use std::{
    collections::HashSet,
    fs::File,
    io::{self, Read},
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::SyncSender as Sender,
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
};

use pest::{error::Error, Position};
use pest_meta::{
    optimizer::OptimizedRule,
    parse_and_optimize,
    parser::{rename_meta_rule, Rule},
};
use pest_vm::Vm;

/// Possible errors that can occur in the debugger context.
#[derive(Debug, thiserror::Error)]
pub enum DebuggerError {
    /// Errors from opening files etc.
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    /// When a filename can't be extracted from a grammar path.
    #[error("Missing filename")]
    MissingFilename,
    /// Running a debugger requires a grammar to be provided.
    #[error("Open grammar first")]
    GrammarNotOpened,
    /// Running a debugger requires a parsing input to be provided.
    #[error("Open input first")]
    InputNotOpened,
    /// Continuing a debugger session requires starting a session by running a rule.
    #[error("Run rule first")]
    RunRuleFirst,
    /// Parsing finished (i.e. cannot continue the session).
    #[error("End-of-input reached")]
    EofReached,
    /// Can't create a `Position` in a given input.
    #[error("Invalid position: {0}")]
    InvalidPosition(usize),
    /// The provided grammar is invalid.
    /// The first element contains a formatted error message.
    /// The second element (`Vec`) contains the errors.
    #[error("Grammar error: {0}")]
    IncorrectGrammar(String, Vec<Error<Rule>>),
    /// When restarting a session, the previous session
    /// seem to have panicked.
    #[error("Previous parsing execution panic: {0}")]
    PreviousRunPanic(String),
}

/// Events that are sent from the debugger.
#[derive(Debug, PartialEq, Eq)]
pub enum DebuggerEvent {
    /// A breakpoint encountered.
    /// The first element is the rule name.
    /// The second element is the position.
    Breakpoint(String, usize),
    /// The end of the input has been reached.
    Eof,
    /// A parsing error encountered.
    Error(String),
}

/// Debugger for pest grammars.
pub struct DebuggerContext {
    handle: Option<JoinHandle<()>>,
    is_done: Arc<AtomicBool>,
    grammar: Option<Vec<OptimizedRule>>,
    input: Option<String>,
    breakpoints: Arc<Mutex<HashSet<String>>>,
}

const POISONED_LOCK_PANIC: &str = "poisoned lock";
const CHANNEL_CLOSED_PANIC: &str = "channel closed";

impl DebuggerContext {
    fn file_to_string(path: impl AsRef<Path>) -> Result<String, DebuggerError> {
        let mut file = File::open(path)?;

        let mut string = String::new();
        file.read_to_string(&mut string)?;

        Ok(string)
    }

    /// Loads a grammar from a file.
    pub fn load_grammar(&mut self, path: impl AsRef<Path>) -> Result<(), DebuggerError> {
        let grammar = DebuggerContext::file_to_string(&path)?;

        let file_name = path
            .as_ref()
            .file_name()
            .map(|string| string.to_string_lossy().into_owned())
            .ok_or(DebuggerError::MissingFilename)?;

        self.grammar = Some(DebuggerContext::parse_grammar(&file_name, &grammar)?);

        Ok(())
    }

    /// Loads a grammar from a string.
    pub fn load_grammar_direct(
        &mut self,
        grammar_name: &str,
        grammar: &str,
    ) -> Result<(), DebuggerError> {
        self.grammar = Some(DebuggerContext::parse_grammar(grammar_name, grammar)?);

        Ok(())
    }

    /// Loads a parsing input from a file.
    pub fn load_input(&mut self, path: impl AsRef<Path>) -> Result<(), DebuggerError> {
        let input = DebuggerContext::file_to_string(path)?;

        self.input = Some(input);

        Ok(())
    }

    /// Loads a parsing input from a string.
    pub fn load_input_direct(&mut self, input: String) {
        self.input = Some(input);
    }

    /// Adds all grammar rules as breakpoints.
    /// This is useful for stepping through the entire parsing process.
    /// It returns an error if the grammar hasn't been loaded yet.
    pub fn add_all_rules_breakpoints(&mut self) -> Result<(), DebuggerError> {
        let ast = self
            .grammar
            .as_ref()
            .ok_or(DebuggerError::GrammarNotOpened)?;
        let mut breakpoints = self.breakpoints.lock().expect(POISONED_LOCK_PANIC);
        for rule in ast {
            breakpoints.insert(rule.name.clone());
        }

        Ok(())
    }

    /// Adds a rule to breakpoints.
    pub fn add_breakpoint(&mut self, rule: String) {
        let mut breakpoints = self.breakpoints.lock().expect(POISONED_LOCK_PANIC);

        breakpoints.insert(rule);
    }

    /// Removes a rule from breakpoints.
    pub fn delete_breakpoint(&mut self, rule: &str) {
        let mut breakpoints = self.breakpoints.lock().expect(POISONED_LOCK_PANIC);

        breakpoints.remove(rule);
    }

    /// Removes all breakpoints.
    pub fn delete_all_breakpoints(&mut self) {
        let mut breakpoints = self.breakpoints.lock().expect(POISONED_LOCK_PANIC);

        breakpoints.clear();
    }

    /// Returns a list of all breakpoints.
    pub fn list_breakpoints(&self) -> Vec<String> {
        let breakpoints = self.breakpoints.lock().expect(POISONED_LOCK_PANIC);
        let mut breakpoints: Vec<_> = breakpoints.iter().map(ToOwned::to_owned).collect();
        breakpoints.sort();
        breakpoints
    }

    fn handle(
        &self,
        ast: Vec<OptimizedRule>,
        rule: String,
        input: String,
        sender: Sender<DebuggerEvent>,
    ) -> JoinHandle<()> {
        let breakpoints = Arc::clone(&self.breakpoints);
        let is_done = Arc::clone(&self.is_done);
        let is_done_signal = Arc::clone(&self.is_done);

        let rsender = sender.clone();
        thread::spawn(move || {
            let vm = Vm::new_with_listener(
                ast,
                Box::new(move |rule, pos| {
                    if is_done_signal.load(Ordering::SeqCst) {
                        return true;
                    }
                    let lock = breakpoints.lock().expect(POISONED_LOCK_PANIC);

                    if lock.contains(&rule) {
                        rsender
                            .send(DebuggerEvent::Breakpoint(rule, pos.pos()))
                            .expect(CHANNEL_CLOSED_PANIC);

                        thread::park();
                    }
                    false
                }),
            );

            match vm.parse(&rule, &input) {
                Ok(_) => sender.send(DebuggerEvent::Eof).expect(CHANNEL_CLOSED_PANIC),
                Err(error) => sender
                    .send(DebuggerEvent::Error(error.to_string()))
                    .expect(CHANNEL_CLOSED_PANIC),
            };

            is_done.store(true, Ordering::SeqCst);
        })
    }

    fn parse_grammar(file_name: &str, grammar: &str) -> Result<Vec<OptimizedRule>, DebuggerError> {
        match parse_and_optimize(grammar) {
            Ok((_, ast)) => Ok(ast),
            Err(errors) => {
                let msg = format!(
                    "error parsing {:?}\n\n{}",
                    file_name,
                    errors
                        .iter()
                        .cloned()
                        .map(|error| format!("{}", error.renamed_rules(rename_meta_rule)))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
                Err(DebuggerError::IncorrectGrammar(msg, errors))
            }
        }
    }

    /// Starts a debugger session: runs a rule on an input and stops at breakpoints.
    /// When the debugger is stopped, an event is sent to the channel using `sender`.
    /// The debugger can be resumed by calling `cont`.
    /// This naturally returns errors if the grammar or input haven't been loaded yet etc.
    pub fn run(&mut self, rule: &str, sender: Sender<DebuggerEvent>) -> Result<(), DebuggerError> {
        if let Some(handle) = self.handle.take() {
            if !(self.is_done.load(Ordering::Relaxed)) {
                self.is_done.store(true, Ordering::SeqCst);
                handle.thread().unpark();
            }
            handle
                .join()
                .map_err(|e| DebuggerError::PreviousRunPanic(format!("{:?}", e)))?;
        }

        self.is_done.store(false, Ordering::SeqCst);
        let ast = self
            .grammar
            .as_ref()
            .ok_or(DebuggerError::GrammarNotOpened)?;
        match self.input {
            Some(ref input) => {
                let rule = rule.to_owned();
                let input = input.clone();

                self.handle = Some(self.handle(ast.clone(), rule, input, sender));
                Ok(())
            }
            None => Err(DebuggerError::InputNotOpened),
        }
    }

    /// Continue the debugger session from the breakpoint.
    /// It returns an error if the session finished or wasn't started yet.
    pub fn cont(&self) -> Result<(), DebuggerError> {
        if self.is_done.load(Ordering::SeqCst) {
            return Err(DebuggerError::EofReached);
        }

        match self.handle {
            Some(ref handle) => {
                handle.thread().unpark();
                Ok(())
            }
            None => Err(DebuggerError::RunRuleFirst),
        }
    }

    /// Returns a `Position` from the loaded input.
    pub fn get_position(&self, pos: usize) -> Result<Position<'_>, DebuggerError> {
        match self.input {
            Some(ref input) => Position::new(input, pos).ok_or(DebuggerError::InvalidPosition(pos)),
            None => Err(DebuggerError::InputNotOpened),
        }
    }
}

impl Default for DebuggerContext {
    fn default() -> Self {
        Self {
            handle: None,
            is_done: Arc::new(AtomicBool::new(false)),
            grammar: None,
            input: None,
            breakpoints: Arc::new(Mutex::new(HashSet::new())),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::sync::mpsc::sync_channel;

    fn get_test_context() -> DebuggerContext {
        let mut context = DebuggerContext::default();

        context
            .load_grammar_direct(
                "testgrammar",
                r#"alpha = { 'a'..'z' | 'A'..'Z' }
            digit = { '0'..'9' }
            
            ident = { !digit ~ (alpha | digit)+ }
            
            ident_list = _{ ident ~ (" " ~ ident)* }"#,
            )
            .expect("Error: failed to load grammar");
        context.load_input_direct("test test2".to_owned());
        context
    }

    #[test]
    fn test_full_flow() {
        let mut context = get_test_context();

        let (sender, receiver) = sync_channel(1);

        assert_eq!(context.list_breakpoints().len(), 0);
        context.add_breakpoint("ident".to_owned());
        assert_eq!(context.list_breakpoints().len(), 1);
        context
            .run("ident_list", sender)
            .expect("Error: failed to run rule");

        let event = receiver.recv().expect("Error: failed to receive event");
        assert_eq!(event, DebuggerEvent::Breakpoint("ident".to_owned(), 0));

        context.cont().expect("Error: failed to continue");

        let event = receiver.recv().expect("Error: failed to receive event");
        assert_eq!(event, DebuggerEvent::Breakpoint("ident".to_owned(), 5));
        context.cont().expect("Error: failed to continue");
        let event = receiver.recv().expect("Error: failed to receive event");

        assert_eq!(event, DebuggerEvent::Eof);
        context
            .add_all_rules_breakpoints()
            .expect("grammar is loaded");
        assert_eq!(context.list_breakpoints().len(), 4);
        context.delete_breakpoint("ident");
        assert_eq!(context.list_breakpoints().len(), 3);
        context.delete_all_breakpoints();
        assert_eq!(context.list_breakpoints().len(), 0);
    }

    #[test]
    fn test_restart() {
        let mut context = get_test_context();

        let (sender, receiver) = sync_channel(1);

        assert_eq!(context.list_breakpoints().len(), 0);
        context.add_breakpoint("ident".to_owned());
        assert_eq!(context.list_breakpoints().len(), 1);
        context
            .run("ident_list", sender)
            .expect("Error: failed to run rule");

        let event = receiver.recv().expect("Error: failed to receive event");
        assert_eq!(event, DebuggerEvent::Breakpoint("ident".to_owned(), 0));
        let (sender2, receiver2) = sync_channel(1);

        context
            .run("ident_list", sender2)
            .expect("Error: failed to run rule");
        let event = receiver2.recv().expect("Error: failed to receive event");
        assert_eq!(event, DebuggerEvent::Breakpoint("ident".to_owned(), 0));
    }

    #[test]
    pub fn test_errors() {
        let mut context = DebuggerContext::default();

        assert!(context.load_input(".").is_err());
        let pest_readme = concat!(env!("CARGO_MANIFEST_DIR"), "/../README.md");
        let pest_grammar = concat!(env!("CARGO_MANIFEST_DIR"), "/../meta/src/grammar.pest");

        assert!(context.load_grammar(pest_readme).is_err());
        assert!(context.add_all_rules_breakpoints().is_err());
        assert!(context.cont().is_err());
        assert!(context.run("rule", sync_channel(1).0).is_err());
        assert!(context.load_grammar(pest_grammar).is_ok());
        assert!(context.run("rule", sync_channel(1).0).is_err());
        assert!(context.get_position(0).is_err());
        context.load_input_direct("".to_owned());
        assert!(context.get_position(0).is_ok());
        assert!(context.get_position(1).is_err());
        assert!(context.load_input(pest_grammar).is_ok());
        let (sender, _receiver) = sync_channel(1);
        assert!(context.run("ANY", sender).is_ok());
        while context.cont().is_ok() {}
        assert!(context.cont().is_err());
    }
}
