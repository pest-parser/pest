#[cfg(not(feature = "std"))]
extern crate alloc;

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::num::NonZeroUsize;
use std::path::Path;
use std::process::{Child, Command, ExitCode, ExitStatus, Stdio};
use std::time::{Duration, Instant};

use pest::Parser;
use wait_timeout::ChildExt;

mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "../examples/base.pest"]
    #[grammar = "../examples/calc.pest"]
    pub struct Calculator;
}

const MAX_INPUT_BYTES: usize = 128 * 1024;
const SYNTAX_ERROR: u8 = 2;
const DEPTH_LIMIT: u8 = 3;
const INPUT_TOO_LARGE: u8 = 4;
const INVALID_UTF8: u8 = 5;
const IO_ERROR: u8 = 6;
const TIMED_OUT: u8 = 124;
const WORKER_FAILED: u8 = 125;

#[derive(Debug)]
enum Outcome {
    Exited(ExitStatus),
    TimedOut,
}

struct Worker(Child);

impl Worker {
    fn spawn(command: &mut Command) -> io::Result<Self> {
        command
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .map(Self)
    }

    fn wait_until(&mut self, deadline: Instant) -> io::Result<Outcome> {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if !remaining.is_zero() {
            if let Some(status) = self.0.wait_timeout(remaining)? {
                if Instant::now() <= deadline {
                    return Ok(Outcome::Exited(status));
                }
            }
        }
        self.terminate()?;
        Ok(Outcome::TimedOut)
    }

    fn terminate(&mut self) -> io::Result<()> {
        if self.0.try_wait()?.is_some() {
            return Ok(());
        }
        if let Err(error) = self.0.kill() {
            return self.0.try_wait()?.map(|_| ()).ok_or(error);
        }
        self.0.wait().map(|_| ())
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        if let Err(error) = self.terminate() {
            eprintln!("failed to clean up parser worker: {error}");
        }
    }
}

fn read_input(reader: impl Read) -> Result<String, u8> {
    let mut bytes = Vec::new();
    reader
        .take((MAX_INPUT_BYTES + 1) as u64)
        .read_to_end(&mut bytes)
        .map_err(|_| IO_ERROR)?;
    if bytes.len() > MAX_INPUT_BYTES {
        return Err(INPUT_TOO_LARGE);
    }
    String::from_utf8(bytes).map_err(|_| INVALID_UTF8)
}

fn parse_input(input: &str) -> u8 {
    pest::set_call_limit(NonZeroUsize::new(50));
    match parser::Calculator::parse(parser::Rule::program, input) {
        Ok(_) => 0,
        Err(error) => match error.variant {
            pest::error::ErrorVariant::CustomError { message }
                if message == "call limit reached" =>
            {
                DEPTH_LIMIT
            }
            _ => SYNTAX_ERROR,
        },
    }
}

fn run_worker(path: &Path) -> u8 {
    match File::open(path).map_err(|_| IO_ERROR).and_then(read_input) {
        Ok(input) => parse_input(&input),
        Err(code) => code,
    }
}

fn run() -> io::Result<u8> {
    let args: Vec<_> = env::args_os().skip(1).collect();
    if let [mode, path] = args.as_slice() {
        if mode == "--worker" {
            return Ok(run_worker(Path::new(path)));
        }
    }
    let [path, timeout_ms] = args.as_slice() else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "usage: parse_with_timeout INPUT_FILE TIMEOUT_MS",
        ));
    };
    let duration = timeout_ms
        .to_str()
        .and_then(|value| value.parse::<u64>().ok())
        .filter(|value| *value > 0)
        .map(Duration::from_millis)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid timeout"))?;
    let deadline = Instant::now()
        .checked_add(duration)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "timeout is too large"))?;
    let mut command = Command::new(env::current_exe()?);
    command.arg("--worker").arg(path);
    let mut worker = Worker::spawn(&mut command)?;
    let code = match worker.wait_until(deadline)? {
        Outcome::TimedOut => TIMED_OUT,
        Outcome::Exited(status) => match status.code() {
            Some(code @ (0 | 2..=6)) => code as u8,
            _ => WORKER_FAILED,
        },
    };
    let message = match code {
        0 => "parsed successfully",
        SYNTAX_ERROR => "invalid expression",
        DEPTH_LIMIT => "parser depth limit reached",
        INPUT_TOO_LARGE => "input exceeds 128 KiB",
        INVALID_UTF8 => "input is not UTF-8",
        IO_ERROR => "could not read input",
        TIMED_OUT => "parser deadline exceeded; worker terminated and reaped",
        _ => "parser worker exited unexpectedly",
    };
    println!("{message}");
    Ok(code)
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => ExitCode::from(code),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(WORKER_FAILED)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MODE: &str = "PEST_TIMEOUT_EXAMPLE_TEST_MODE";
    const CHILD_EXIT_BASE: i32 = 40;

    fn child(mode: &str) -> Worker {
        let mut command = Command::new(env::current_exe().unwrap());
        command
            .args(["--exact", "tests::worker_process", "--ignored"])
            .env(MODE, mode);
        Worker::spawn(&mut command).unwrap()
    }

    fn assert_completes(mode: &str, expected: u8) {
        let mut worker = child(mode);
        let outcome = worker
            .wait_until(Instant::now() + Duration::from_secs(10))
            .unwrap();
        let Outcome::Exited(status) = outcome else {
            panic!("worker unexpectedly timed out: {mode}");
        };
        assert_eq!(status.code(), Some(CHILD_EXIT_BASE + i32::from(expected)));
        assert!(worker.0.try_wait().unwrap().is_some());
    }

    #[test]
    #[ignore = "subprocess helper for the supervisor tests"]
    fn worker_process() {
        let code = match env::var(MODE).unwrap().as_str() {
            "valid" => parse_input("(1 + 2) * 3"),
            "invalid" => parse_input("1 +"),
            "deep" => parse_input(&format!("{}1{}", "(".repeat(300), ")".repeat(300))),
            "failed" => WORKER_FAILED,
            "busy" => loop {
                std::hint::spin_loop();
            },
            _ => panic!("unknown worker test mode"),
        };
        std::process::exit(CHILD_EXIT_BASE + i32::from(code));
    }

    #[test]
    fn distinguishes_worker_results() {
        for (mode, code) in [
            ("valid", 0),
            ("invalid", SYNTAX_ERROR),
            ("deep", DEPTH_LIMIT),
            ("failed", WORKER_FAILED),
        ] {
            assert_completes(mode, code);
        }
    }

    #[test]
    fn timeout_terminates_and_reaps_worker() {
        for _ in 0..3 {
            let deadline = Instant::now() + Duration::from_millis(100);
            let mut worker = child("busy");
            assert!(matches!(
                worker.wait_until(deadline).unwrap(),
                Outcome::TimedOut
            ));
            assert!(worker.0.try_wait().unwrap().is_some());
            assert!(Instant::now() < deadline + Duration::from_secs(5));
            assert_completes("valid", 0);
        }
    }

    #[test]
    fn cancellation_terminates_and_reaps_worker() {
        let mut worker = child("busy");
        worker.terminate().unwrap();
        assert!(worker.0.try_wait().unwrap().is_some());
        worker.terminate().unwrap();
    }

    #[test]
    fn input_read_is_bounded() {
        assert_eq!(read_input(&b"1 + 2"[..]), Ok("1 + 2".to_owned()));
        assert_eq!(read_input(&b"\xff"[..]), Err(INVALID_UTF8));
        assert_eq!(read_input(io::repeat(b'1')), Err(INPUT_TOO_LARGE));
        assert_eq!(
            read_input(&vec![b'1'; MAX_INPUT_BYTES][..]).unwrap().len(),
            MAX_INPUT_BYTES
        );
    }
}
