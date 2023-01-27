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
//! This crate contains the CLI debugger.

#![doc(
    html_logo_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/pest-parser/pest/master/pest-logo.svg"
)]
#![warn(missing_docs, rust_2018_idioms, unused_qualifications)]
use std::path::PathBuf;
use std::sync::mpsc::{self, Receiver};
use std::time::Duration;

use pest::error::{Error, ErrorVariant};

use pest_debugger::{DebuggerContext, DebuggerError, DebuggerEvent};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Default)]
struct Cli {
    context: DebuggerContext,
    receiver: Option<Receiver<DebuggerEvent>>,
}

impl Cli {
    fn grammar(&mut self, path: PathBuf) -> Result<(), DebuggerError> {
        self.context.load_grammar(&path)
    }

    fn input(&mut self, path: PathBuf) -> Result<(), DebuggerError> {
        self.context.load_input(&path)
    }

    fn breakpoint(&mut self, rule: &str) {
        self.context.add_breakpoint(rule.to_owned());
    }

    fn run(&mut self, rule: &str) -> Result<(), DebuggerError> {
        let (sender, receiver) = mpsc::sync_channel(1);
        let rec = &receiver;
        self.context.run(rule, sender)?;
        match rec.recv_timeout(Duration::from_secs(5)) {
            Ok(DebuggerEvent::Breakpoint(rule, pos)) => {
                let error: Error<()> = Error::new_from_pos(
                    ErrorVariant::CustomError {
                        message: format!("parsing {}", rule),
                    },
                    self.context.get_position(pos)?,
                );
                println!("{}", error);
            }
            Ok(DebuggerEvent::Eof) => println!("end-of-input reached"),
            Ok(DebuggerEvent::Error(error)) => println!("{}", error),
            Err(_) => eprintln!("parsing timed out"),
        }
        self.receiver = Some(receiver);
        Ok(())
    }

    fn cont(&mut self) -> Result<(), DebuggerError> {
        self.context.cont()?;

        match self.receiver {
            Some(ref rec) => match rec.recv_timeout(Duration::from_secs(5)) {
                Ok(DebuggerEvent::Breakpoint(rule, pos)) => {
                    let error: Error<()> = Error::new_from_pos(
                        ErrorVariant::CustomError {
                            message: format!("parsing {}", rule),
                        },
                        self.context.get_position(pos)?,
                    );
                    println!("{}", error);
                }
                Ok(DebuggerEvent::Eof) => println!("end-of-input reached"),
                Ok(DebuggerEvent::Error(error)) => println!("{}", error),
                Err(_) => eprintln!("parsing timed out"),
            },
            None => println!("Error: run rule first"),
        };
        Ok(())
    }

    fn list(&mut self) {
        let breakpoints = self.context.list_breakpoints();

        println!("Breakpoints: {}", breakpoints.join(", "));
    }

    fn help() {
        println!(
            "\n\
             Use the following commands:\n\
             g <grammar filename>          - load .pest grammar\n\
             i <input filename>            - load input from a file\n\
             id <input text>               - load input directly from a single-line input\n\
             ba                            - add breakpoints at all rules\n\
             b <rule>                      - add a breakpoint at a rule\n\
             d <rule>                      - delete a breakpoint at a rule\n\
             da                            - delete all breakpoints\n\
             r <rule>                      - run a rule\n\
             c                             - continue\n\
             l                             - list breakpoints\n\
             h                             - help\n\
         "
        );
    }

    fn unrecognized(command: &str) {
        println!("Unrecognized command: {}; use h for help", command);
    }

    fn execute_command(&mut self, command: &str) -> Result<(), DebuggerError> {
        match command {
            "" => (),
            "h" => Cli::help(),
            "l" => self.list(),
            "c" => self.cont()?,
            "ba" => self.context.add_all_rules_breakpoints()?,
            "da" => self.context.delete_all_breakpoints(),
            x if x.starts_with("g ") => self.grammar(PathBuf::from(&x[2..]))?,
            x if x.starts_with("i ") => self.input(PathBuf::from(&x[2..]))?,
            x if x.starts_with("id ") => {
                let input = &x[3..];
                self.context.load_input_direct(input.to_owned());
            }
            x if x.starts_with("b ") => self.breakpoint(&x[2..]),
            x if x.starts_with("d ") => {
                self.context.delete_breakpoint(&x[2..]);
            }
            x if x.starts_with("r ") => self.run(&x[2..])?,
            x => Cli::unrecognized(x),
        };
        Ok(())
    }
}

struct CliHelper {
    completer: FilenameCompleter,
    hinter: HistoryHinter,
}

impl Validator for CliHelper {}
impl Highlighter for CliHelper {}
impl Helper for CliHelper {}

impl Hinter for CliHelper {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Completer for CliHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        self.completer.complete_path(line, pos)
    }
}

struct CliArgs {
    grammar_file: Option<PathBuf>,
    input_file: Option<PathBuf>,
    rule: Option<String>,
    breakpoint: Option<String>,
    session_file: Option<PathBuf>,
}

impl Default for CliArgs {
    fn default() -> Self {
        let mut result = Self {
            grammar_file: None,
            input_file: None,
            rule: None,
            breakpoint: None,
            session_file: None,
        };
        let args = std::env::args();
        let mut iter = args.skip(1);
        let mut unexpected_arg = false;
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "-g" | "--grammar" => {
                    if let Some(grammar_file) = iter.next() {
                        result.grammar_file = Some(PathBuf::from(grammar_file));
                    } else {
                        eprintln!("Error: missing grammar file");
                        std::process::exit(1);
                    }
                }
                "-i" | "--input" => {
                    if let Some(input_file) = iter.next() {
                        result.input_file = Some(PathBuf::from(input_file));
                    } else {
                        eprintln!("Error: missing input file");
                        std::process::exit(1);
                    }
                }
                "-r" | "--rule" => {
                    if let Some(rule) = iter.next() {
                        result.rule = Some(rule);
                    } else {
                        eprintln!("Error: missing rule");
                        std::process::exit(1);
                    }
                }
                "-b" | "--breakpoint" => {
                    if let Some(breakpoint) = iter.next() {
                        result.breakpoint = Some(breakpoint);
                    } else {
                        eprintln!("Error: missing breakpoint");
                        std::process::exit(1);
                    }
                }
                "-s" | "--session" => {
                    if let Some(session_file) = iter.next() {
                        result.session_file = Some(PathBuf::from(session_file));
                    } else {
                        eprintln!("Error: missing session file");
                        std::process::exit(1);
                    }
                }
                "-h" | "--help" => {
                    println!(
                        "\n\
                         Usage: pest_debugger [options]\n\
                         \n\
                         Options:\n\
                         -g, --grammar <grammar file>    - load .pest grammar\n\
                         -i, --input <input file>        - load input file\n\
                         -r, --rule <rule>               - run rule\n\
                         -b, --breakpoint <rule>         - breakpoint at rule\n\
                         -s, --session <session file>    - load session history file\n\
                         -h, --help                      - print this help menu\n\
                     "
                    );
                    std::process::exit(0);
                }
                arg => {
                    eprintln!("Error: unexpected argument `{}`", arg);
                    unexpected_arg = true;
                }
            }
        }
        if unexpected_arg {
            std::process::exit(1);
        }
        result
    }
}

impl CliArgs {
    fn init(self, context: &mut Cli) {
        if let Some(grammar_file) = self.grammar_file {
            if let Err(e) = context.grammar(grammar_file) {
                eprintln!("Error: {}", e);
            }
        }
        if let Some(input_file) = self.input_file {
            if let Err(e) = context.input(input_file) {
                eprintln!("Error: {}", e);
            }
        }
        if let Some(breakpoint) = &self.breakpoint {
            context.breakpoint(breakpoint);
        }
        if let Some(rule) = self.rule {
            if let Err(e) = context.run(&rule) {
                eprintln!("Error: {}", e);
            }
        }
    }
}

fn main() -> rustyline::Result<()> {
    let mut rl = Editor::<CliHelper>::new()?;
    let mut context = Cli::default();
    let cli_args = CliArgs::default();

    let h = CliHelper {
        completer: FilenameCompleter::new(),
        hinter: HistoryHinter {},
    };
    rl.set_helper(Some(h));
    println!("pest_debugger v{}\n", VERSION);
    let historyfile = if let Some(session_file) = &cli_args.session_file {
        if let Err(e) = rl.load_history(session_file) {
            eprintln!("Error loading history file: {}", e);
        }
        Some(session_file.clone())
    } else {
        None
    };
    cli_args.init(&mut context);
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(line.clone());
                if let Err(err) = context.execute_command(line.trim()) {
                    println!("Error: {}", err);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    if let Some(historyfile) = historyfile {
        if let Err(e) = rl.save_history(&historyfile) {
            eprintln!("Error saving history file: {}", e);
        }
    }
    Ok(())
}
