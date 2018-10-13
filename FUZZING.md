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

- `toml`
- `json`

There are two fuzzing targets for this crate: one tests the json grammar in the
`json` module and the other tests the toml grammar in the `toml` module. They
interact directly with the `pest::Parser::parse` function provided by derived
on the respective Parsers in each module.

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

For more information, run `cargo fuzz -h` or check out the `cargo-fuzz`
project linked above.

