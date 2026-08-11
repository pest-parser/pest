# Fuzzing pest

There are currently fuzzing targets set up on two of the crates used in `pest`:
`pest_grammars` and `pest_meta`. These targets serve to [fuzz test] the parsing
provided by these crates by providing random inputs and searching for crashes.
The [cargo-fuzz] tool is used to run `libFuzzer` on the targets.

[fuzz test]: https://en.wikipedia.org/wiki/Fuzzing
[cargo-fuzz]: https://github.com/rust-fuzz/cargo-fuzz

## Fuzz targets

Each of the crates fuzzed has its targets held in the `fuzz` directory. 

### `pest_meta`

- `parser`

There is a single fuzzing target for this crate that interacts with
`pest_meta::parser::parse`, a function that parses pest grammar files.

### `pest_grammars`

- `http`
- `toml`
- `json`
- `sql`

There are four fuzzing targets for this crate: one tests the http request grammar in the `http` module, one tests the json grammar in the `json` module, one tests the toml grammar in the `toml` module, and the last one tests the sql grammar in the `sql` module. They interact directly with the `pest::Parser::parse` function provided by derived
on the respective Parsers in each module.

## Dictionaries

Dictionaries of syntax tokens for the `pest_grammars` fuzz targets live in `grammars/fuzz/dict`. Passing one dictionary to libFuzzer lets it splice whole keywords and punctuation into inputs instead of rediscovering them one byte at a time.

## Running a target

In order to run a fuzz target, first install cargo-fuzz:

```sh
> cargo install cargo-fuzz
```

Next, make sure that you are in the sub-directory corresponding to one of the
crates with fuzzing targets. Once there, use rustup (or whatever tool you use to
manage rustc versions) to use the `nightly` compiler.

Once using the nightly compiler, build and run the fuzzing crate, where target
is the fuzzing target to run:

```sh
> cargo fuzz run [target]
```

> When compiling the fuzzing crate for the first time, it fails with an error
> about the lack of the dynamic library `proc_macro` (at least on macOS). To fix
> this error, simply run `cargo fuzz` again.

Additionally, you can run the fuzz target together with its dictionary with the following command:

```sh
> cargo fuzz run [target] -- -dict=fuzz/dict/[target].dict 
```

The path is relative to the directory you run the command from. libFuzzer
reports how many entries it loaded at startup.

For more information, run `cargo fuzz -h` or check out the `cargo-fuzz`
project linked above.

