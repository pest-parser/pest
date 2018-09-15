extern crate pest;
extern crate pest_meta;
extern crate pest_vm;
extern crate rustyline;

use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::{self, JoinHandle};

use pest::error::{Error, ErrorVariant};

use pest_meta::parse_and_optimize;
use pest_meta::optimizer::OptimizedRule;

use pest_vm::Vm;

use rustyline::error::ReadlineError;
use rustyline::Editor;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

struct Context {
    handle: Option<JoinHandle<()>>,
    is_done: Arc<AtomicBool>,
    grammar: Option<(String, String)>,
    input: Option<String>,
    breakpoints: Arc<Mutex<HashSet<String>>>
}

impl Context {
    pub fn new() -> Context {
        Context {
            handle: None,
            is_done: Arc::new(AtomicBool::new(false)),
            grammar: None,
            input: None,
            breakpoints: Arc::new(Mutex::new(HashSet::new()))
        }
    }

    fn file_to_string(path: &str) -> Result<String, io::Error> {
        let mut file = File::open(path)?;

        let mut string = String::new();
        file.read_to_string(&mut string)?;

        return Ok(string);
    }

    fn grammar(&mut self, path: &str) -> Result<(), io::Error> {
        let grammar = Context::file_to_string(path)?;

        let path = Path::new(path);
        let file_name = path.file_name().map(|string| {
            string.to_string_lossy().into_owned()
        }).unwrap();

        self.grammar = Some((file_name, grammar));

        Ok(())
    }

    fn input(&mut self, path: &str) -> Result<(), io::Error> {
        self.input = Some(Context::file_to_string(path)?);

        Ok(())
    }

    fn breakpoint(&mut self, rule: &str) {
        self.breakpoints.lock().unwrap().insert(rule.to_owned());
    }

    fn handle(&self, ast: Vec<OptimizedRule>, rule: String, input: String) -> JoinHandle<()> {
        let breakpoints = Arc::clone(&self.breakpoints);
        let is_done = Arc::clone(&self.is_done);

        thread::spawn(move || {
            let vm = Vm::with_listener(ast, Box::new(move |rule, pos| {
                let lock = breakpoints.lock().unwrap();

                if lock.contains(&rule) {
                    let error: Error<()> = Error::new_from_pos(
                        ErrorVariant::CustomError {
                            message: format!("parsing {}", rule)
                        },
                        pos.clone()
                    );
                    println!("{}", error);

                    thread::park();
                }
            }));

            match vm.parse(&rule, &input) {
                Ok(_) => println!("end-of-input reached"),
                Err(error) => println!("{}", error)
            };

            is_done.store(true, Ordering::SeqCst);
        })
    }

    fn run(&mut self, rule: &str) {
        if self.is_done.load(Ordering::SeqCst) {
            if let Some(handle) = self.handle.take() {
                handle.join().unwrap();
            }

            self.is_done.store(false, Ordering::SeqCst);
        }

        match self.grammar {
            Some((ref file_name, ref grammar)) => {
                match parse_and_optimize(file_name, grammar) {
                    Ok((_, ast)) => {
                        match self.input {
                            Some(ref input) => {
                                let rule = rule.to_owned();
                                let input = input.clone();

                                self.handle = Some(self.handle(ast, rule, input));
                            }
                            None => {
                                println!("Error: open input first");
                                return;
                            }
                        }
                    }
                    Err(ref errors) => {
                        println!(
                            "Grammar errors:\n\n{}",
                            errors.into_iter()
                                .map(|error| format!("{}", error))
                                .collect::<Vec<_>>()
                                .join("\n\n")
                        );
                        return;
                    }
                }
            }
            None => {
                println!("Error: open grammar first");
                return;
            }
        }
    }

    fn cont(&self) {
        if self.is_done.load(Ordering::SeqCst) {
            println!("end-of-input reached");
            return;
        }

        match self.handle {
            Some(ref handle) => {
                handle.thread().unpark();
            }
            None => println!("Error: run rule first")
        };
    }

    fn list(&self) {
        let lock = self.breakpoints
            .lock()
            .unwrap();
        let mut breakpoints: Vec<_> = lock
            .iter()
            .map(String::as_ref)
            .collect();
        breakpoints.sort();

        println!("Breakpoints: {}", breakpoints.join(", "));
    }

    fn help() {
        println!("\n\
            Use the following commands:\n\
            g <grammar> - load .pest grammar\n\
            i <input>   - load input file\n\
            b <rule>    - breakpoint at rule\n\
            r <rule>    - run rule\n\
            c           - continue\n\
            l           - list breakpoints\n\
            h           - help\n\
        ");
    }

    fn unrecognized(command: &str) {
        println!("Unrecognized command: {}; use h for help", command);
    }

    fn execute_command(&mut self, command: &str) -> Result<(), io::Error> {
        let tokens: Vec<_> = command.split(" ").collect();

        match &tokens[..] {
            [""] => (),
            ["g", path] => self.grammar(path)?,
            ["i", path] => self.input(path)?,
            ["b", rule] => self.breakpoint(rule),
            ["r", rule] => self.run(rule),
            ["c"] => self.cont(),
            ["l"] => self.list(),
            ["h"] => Context::help(),
            _ => Context::unrecognized(command)
        };

        Ok(())
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    let mut context = Context::new();

    println!("pest_debugger v{}\n", VERSION);

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
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
}
