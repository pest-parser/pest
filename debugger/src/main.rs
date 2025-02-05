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

use pest::TracingType;
use pest_debugger::{DebuggerContext, DebuggerError, DebuggerEvent};
use reqwest::blocking::{Client, ClientBuilder};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};

use clap::{CommandFactory, Parser};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct ClapCliArgs {
    /// Select the grammar
    #[arg(short, long, alias = "grammar")]
    grammar_file: Option<PathBuf>,

    /// Select the input file
    #[arg(short, long, alias = "input")]
    input_file: Option<PathBuf>,

    /// Select the session file
    #[arg(short, long, alias = "session")]
    session_file: Option<PathBuf>,

    /// Select the start rule
    #[arg(short, long)]
    rule: Option<String>,

    /// Select initial breakpoint rules (takes a space separated list)
    #[arg(short, long, alias = "breakpoint")]
    breakpoints: Vec<String>,

    /// Don't check for updates
    #[arg(short, long)]
    no_update: bool,

    /// Turn on tracing (string should be PegViz or Indented)
    #[arg(short, long)]
    tracing: Option<String>,

    /// Set indent depth for Indented tracing
    #[arg(long)]
    tracing_spacing: Option<usize>,

    /// Do not show tracing lines for implicit whitespace and comments
    #[arg(long)]
    tracing_skip_implicit: bool,

    /// Do not show tracing lines for silent rules
    #[arg(long)]
    tracing_skip_silent: bool,
}

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct ClapRLArgs {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(clap::Subcommand)]
#[clap(disable_help_subcommand = true)]
#[clap(disable_help_flag = true)]
#[clap(disable_version_flag = true)]
#[clap(infer_subcommands = true)]
enum Commands {
    /// Select the grammar
    Grammar { grammar: String },
    /// Select the input file
    Input { input: String },
    /// Run starting at the given rule
    #[clap(visible_alias("rule"))]
    Run { run: String },
    /// Add breakpoints (space separated)
    Breakpoints {
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        breakpoints: Vec<String>,
    },
    /// Give an input string
    #[clap(visible_alias("text_input"))]
    Id {
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },
    /// Set all breakpoints
    Ba {},
    /// Delete breakpoints (space separated)
    Delete {
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        delete: Vec<String>,
    },
    /// Delete all breakpoints
    Da {},
    /// List all breakpoints
    List {},
    /// Continue the given number of steps (default 1)
    Continue { count: Option<u32> },
    // We have to do help ourselves so it doesn't exit :D
    /// Show help
    Help {},
}

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

    fn tracing(&mut self, ttype: &str) {
        let ttype = match ttype.to_lowercase().as_str() {
            "pegviz" => TracingType::PegViz,
            "indented" => TracingType::Indented,
            "none" => TracingType::None,
            _ => {
                eprintln!("Bad tracing type {ttype}!");
                return;
            }
        };
        self.context.tracing(ttype);
    }

    fn tracing_spacing(&mut self, size: usize) {
        self.context.tracing_spacing(size);
    }

    fn tracing_skip_implicit(&mut self) {
        self.context.tracing_skip_implicit();
    }

    fn tracing_skip_silent(&mut self) {
        self.context.tracing_skip_silent();
    }

    fn run(&mut self, rule: &str) -> Result<(), DebuggerError> {
        let (sender, receiver) = mpsc::sync_channel(1);
        let rec = &receiver;
        self.context.run(rule, sender)?;
        match rec.recv_timeout(Duration::from_secs(30)) {
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
            Some(ref rec) => match rec.recv_timeout(Duration::from_secs(30)) {
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

    fn execute_command(&mut self, command: &str) -> Result<(), DebuggerError> {
        // First "argument" needs to be the program name; we don't care so empty string
        let mut words = vec![""];
        for word in command.split_whitespace() {
            words.push(word);
        }
        let args = ClapRLArgs::parse_from(&words);
        match &args.command {
            // We have to do help ourselves so it doesn't exit :D
            Some(Commands::Help {}) => {
                println!("{}", ClapRLArgs::command().render_long_help());
            }
            Some(Commands::Grammar { grammar }) => {
                self.grammar(PathBuf::from(grammar))?;
            }
            Some(Commands::Input { input }) => {
                self.input(PathBuf::from(input))?;
            }
            Some(Commands::Run { run }) => {
                self.run(run)?;
            }
            Some(Commands::Ba {}) => self.context.add_all_rules_breakpoints()?,
            Some(Commands::Da {}) => self.context.delete_all_breakpoints(),
            Some(Commands::Continue { count }) => {
                if let Some(x) = count {
                    for _ in 0..*x {
                        self.cont()?
                    }
                } else {
                    self.cont()?
                }
            }
            // NOTE: It seems possible that there are conditions under which this doesn't work
            // because the "arguments" are not ones that Clap likes; if that happens, just pull the
            // `id string` handling out of here.  Just check that the string starts with "id " and
            // keep the rest.
            Some(Commands::Id { args }) => {
                self.context.load_input_direct(args.join(" ").to_owned());
            }
            Some(Commands::Breakpoints { breakpoints }) => {
                for rule in breakpoints {
                    self.breakpoint(rule);
                }
            }
            Some(Commands::Delete { delete }) => {
                for rule in delete {
                    self.context.delete_breakpoint(rule);
                }
            }
            Some(Commands::List {}) => {
                self.list();
            }
            None => {}
        }
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

fn main() -> rustyline::Result<()> {
    let mut rl = Editor::<CliHelper>::new()?;
    let mut context = Cli::default();
    let cli_args = ClapCliArgs::parse();

    if !cli_args.no_update {
        let client = ClientBuilder::new()
            .user_agent(concat!(
                env!("CARGO_PKG_NAME"),
                "/",
                env!("CARGO_PKG_VERSION")
            ))
            .timeout(Some(Duration::from_secs(30)))
            .build()
            .ok();

        if let Some(client) = client {
            if let Some(new_version) = check_for_updates(client) {
                println!(
                    "A new version of pest_debugger is available: v{}",
                    new_version
                );
            } else {
                println!("pest_debugger is up to date.");
            }
        }
    }

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

    if let Some(grammar_file) = cli_args.grammar_file {
        if let Err(e) = context.grammar(grammar_file) {
            eprintln!("Error: {}", e);
        }
    }
    if let Some(input_file) = cli_args.input_file {
        if let Err(e) = context.input(input_file) {
            eprintln!("Error: {}", e);
        }
    }
    // The list is generated by one or more -b arguments, but each -b argument can also be a
    // space-separated list
    for breakpoint_list in cli_args.breakpoints {
        for breakpoint in breakpoint_list.split_whitespace() {
            context.breakpoint(breakpoint);
        }
    }
    if let Some(rule) = cli_args.rule {
        if let Err(e) = context.run(&rule) {
            eprintln!("Error: {}", e);
        }
    }

    if let Some(tracing) = cli_args.tracing {
        context.tracing(&tracing);
    }

    if let Some(size) = cli_args.tracing_spacing {
        context.tracing_spacing(size);
    }

    if cli_args.tracing_skip_implicit {
        context.tracing_skip_implicit();
    }

    if cli_args.tracing_skip_silent {
        context.tracing_skip_silent();
    }

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

fn check_for_updates(client: Client) -> Option<String> {
    let response = client
        .get("https://crates.io/api/v1/crates/pest_debugger")
        .send();

    if let Ok(response) = response {
        response.json::<serde_json::Value>().ok().and_then(|json| {
            let version = json["crate"]["max_version"].as_str()?;

            if version != VERSION {
                Some(version.to_string())
            } else {
                None
            }
        })
    } else {
        None
    }
}
