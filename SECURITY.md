# Security Policy

## Supported Versions

Only the most recent minor version is supported.

| Version | Supported          |
| ------- | ------------------ |
| 2.9.x   | :white_check_mark: |
| < 2.9.x | :x:                |

## Parsing Untrusted Input

Applications should choose resource limits based on their expected workloads.
When accepting untrusted or attacker-controlled input, enforce suitable input,
work, memory, and concurrency limits even if the grammar is trusted and
developer-authored. Use supervised parsing when an elapsed-time deadline or
cancellation must be enforced; process isolation is not required for every parse.

Small inputs can require substantial parsing time when a grammar repeatedly
backtracks. Limiting input size or recursion depth alone does not bound this work.
Treat time, counted parser calls, memory, and request concurrency as separate
resource limits.

`pest::set_call_limit` sets a cumulative budget for counted parser-state helper
entries. Returning, backtracking, and stack restoration do not refund calls.
Repetition counts its entry, not every iteration; primitive matching/scanning and
arbitrary Rust callbacks may perform substantial work without another check.
The budget defaults to unlimited and is captured when a parser state is created;
changing it cannot cancel an existing parse. It is not a wall-clock deadline,
instruction-work budget, or memory bound.

Supported `std` builds automatically check native stack headroom at parser
checkpoints, independently of the call budget. Insufficient measurable space
returns "stack limit reached". This is best-effort: unknown stack bounds retain
normal parsing behavior, and `no_std` builds have no native stack check. The
probe is amortized while measured headroom exceeds 1 MiB; once a probe reports
at most 1 MiB, subsequent entries are probed individually. The rejection reserve
is 64 KiB. These are implementation
parameters validated against supported builds, not portable depth guarantees. The
reserve depends on supported runtime/build assumptions and cannot protect
arbitrary callbacks, unusually large frames between checkpoints, or foreign stack
switching. Do not treat it as a universal stack-overflow guarantee.

Grammar validation, AST construction, optimization, and application processing
are not all governed by parser-state limits. Pest's synchronous `Parser::parse`
API does not expose a deadline or cooperative cancellation hook.

### Enforcing a Deadline

For adversarial input that must be stopped, run parsing in a supervised child
process. Include AST construction and validation in that process if they can
also require excessive work or stack space.

1. Use one absolute monotonic deadline covering queueing, input transfer, parsing,
	and result collection. Keep the supervisor outside the CPU-bound computation.
2. On expiry or caller cancellation, terminate the worker and wait for its exit
	before releasing its capacity. Discard partial results and do not automatically
	retry the same input. Report timeout, syntax error, and worker failure separately.
3. Bound active workers, queued jobs, request rate, input bytes, and result/log
	sizes. Use one active job per worker so termination does not cancel unrelated
	requests. When using pipes, drain them concurrently with bounded buffering.
4. Apply operating-system memory and CPU limits where needed. A CPU-time limit
	or quota is not a wall-clock deadline, and may cover a persistent worker's
	entire lifetime. Killing a single process does not kill its descendants;
	use process groups or job objects if workers can create other processes.
5. Choose deadlines from legitimate worst-case workloads and the application's
	latency requirements, not just input length. Account for scheduling and cleanup
	latency rather than promising an exact real-time cutoff.

The supervisor must own cleanup even if the request handler is cancelled. With
`std::process::Child`, call `kill()` and then `wait()`; dropping a child handle
does not terminate or reap the process. Handle cleanup errors instead of claiming
that work has stopped. In Tokio, `Child::kill().await` also waits for termination;
`Command::kill_on_drop(true)` is an additional safeguard, not a substitute for
explicit lifecycle management.

### What Does Not Stop Parsing

- A `tokio::time::timeout` around a synchronous parse inside an async block cannot
  preempt that non-yielding computation. Even the timeout may not be observed until
  parsing returns.
- Timing out a `spawn_blocking` join handle stops waiting, but the started work
  continues. Calling `abort()` cannot stop a started blocking task.
- A channel receive timeout or dropping a thread handle does not stop its thread.
  `catch_unwind` is not a timeout mechanism or protection against stack-overflow
  process aborts.

See Tokio's [timeout documentation](https://docs.rs/tokio/latest/tokio/time/fn.timeout.html)
and [blocking-task cancellation documentation](https://docs.rs/tokio/latest/tokio/task/fn.spawn_blocking.html).

### Runnable Example

The [supervised parser example](derive/examples/parse_with_timeout.rs) reuses the
calculator grammar. From a repository checkout:

```sh
printf '%s\n' '(1 + 2) * 3' > expression.txt
cargo run -p pest_derive --example parse_with_timeout -- expression.txt 1000
```

The second argument is the timeout in milliseconds. The deadline starts before
spawning the worker and covers opening and reading the file, parsing, and observing
exit; it does not include Cargo compilation or creating the input file. The worker
reads at most 128 KiB plus one byte and returns only an exit status. It leaves the
cumulative call budget disabled to demonstrate independent deadline supervision;
automatic native-stack checking still applies where supported. Its standard
streams are disconnected, so there are no output pipes to fill or unbounded parse
diagnostics to collect. The input cap and chosen deadline are illustrative, not
universally appropriate defaults.

Exit codes are `0` for success, `2` for syntax error, `3` for native stack shortage,
`4` for oversized input, `5` for invalid UTF-8, `6` for an input I/O error,
`124` for timeout after cleanup, and `125` for a supervisor or unexpected worker
failure. The `--worker` mode is internal; expose only the supervised entry point
to callers. No input is passed through a shell.

This is a one-shot example, not a service or a sandbox. A service still needs
bounded admission, worker-memory limits, request-cancellation handling, and a
supervisor lifecycle independent of requests. The cleanup guard handles normal
returns and unwinding, but cannot run if the parent is forcibly killed or aborts.
The example uses [wait-timeout](https://docs.rs/wait-timeout/) as a dev dependency
only; on Unix it installs a `SIGCHLD` handler, which can conflict with an
application's own signal handling. It does not belong inside an async executor
thread; use the executor's process API for an async supervisor.

Run its lifecycle tests in both build modes:

```sh
cargo test -p pest_derive --example parse_with_timeout
cargo test -p pest_derive --example parse_with_timeout --release
```

The tests use isolated, non-cooperative workers to check timeout and cancellation
cleanup, repeated timeout recovery, distinct parse outcomes, and bounded input
reads. When adapting the pattern, also test caller disconnects, queue saturation,
bounded IPC, crashes, and process cleanup on each supported operating system.
Retain adversarial grammar benchmarks: process supervision contains expensive work
but does not improve the grammar's complexity. Cumulative call budgets constrain
guarded backtracking, not every unit of computation or elapsed time.

## Reporting a Vulnerability

Please use the [GitHub private reporting functionality](https://github.com/pest-parser/pest/security/advisories/new)
to submit potential security bug reports. If the bug report is reproduced and valid, we'll then:

- Prepare a fix and regression tests.
- Make a patch release for the most recent release.
- Submit an advisory to [rustsec/advisory-db](https://github.com/RustSec/advisory-db).
- Refer to the advisory in the release notes.

If you're *looking* for security bugs, [this crate is set up for
`cargo fuzz`](https://github.com/pest-parser/pest/blob/master/FUZZING.md) but would benefit from more runtime, targets and corpora.
