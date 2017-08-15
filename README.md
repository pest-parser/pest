<p align="center">
  <img src="https://cdn.rawgit.com/dragostis/pest/9605855a9c4e86d5e89bc963fe50ee344d8a40e6/pest-logo.svg"/>
</p>

# pest. The Elegant Parser <sup>(beta)</sup>

[![Join the chat at https://gitter.im/dragostis/pest](https://badges.gitter.im/dragostis/pest.svg)](https://gitter.im/dragostis/pest?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/pest-parser/pest.svg?branch=master)](https://travis-ci.org/pest-parser/pest)
[![codecov](https://codecov.io/gh/pest-parser/pest/branch/master/graph/badge.svg)](https://codecov.io/gh/pest-parser/pest)
[![Crates.io](https://img.shields.io/crates/v/pest.svg)](https://crates.io/crates/pest)
[![Crates.io](https://img.shields.io/crates/d/pest.svg)](https://crates.io/crates/pest)
[![Docs](https://docs.rs/pest/badge.svg)](https://docs.rs/pest)

pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser with *simplicity* and *speed* in mind.

## Usage
You will need [Cargo and Rust](https://www.rust-lang.org/en-US/downloads.html).

Add the following to `Cargo.toml`:

```toml
pest = "1.0-beta.1"
pest_derive = "1.0-beta.1"
```

and in your Rust `lib.rs` or `main.rs`:

```rust
extern crate pest;
#[macro_use]
extern crate pest_derive;
```

## Example

Grammar is specified in a `paren.pest` file:

```
expression = _{ paren ~ expression? }
paren      =  { "(" ~ expression? ~ ")" }
```

and then used in Rust:

```rust
#[derive(Parser)]
#[grammar = "paren.pest"]
struct ParenParser;

assert!(ParenParser::parse_str("((())())").is_ok());
```
## Features

* simple yet elegant [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) grammar
* smart, out-of-the-box error reporting
* precedence climbing API
* no generation step (one-step compilation)
* fast macro-generated recursive descent parser
* runs on stable Rust
* elegant `Iterator`-inspired token pair processing

## Comparison

Short comparison with other parsing tools. Please take into consideration that
pest is the youngest among them, but is continuously improving.

|                      | [nom][1]             | [LALRPOP][2]    | pest               |
|----------------------|----------------------|-----------------|--------------------|
| **type**             | combinator           | generator       | generator (macros) |
| **goals**            | speed, extensibility | usability       | simplicity, speed  |
| **grammar**          | specialized          | LR(1) / LALR(1) | PEG                |
| **steps**            | 1                    | 2               | 1                  |
| **code**             | separate / mixed     | mixed           | separate           |
| **extensibility**    | great                | great           | little             |
| **great for**        | binary formats       | any text        | languages          |
| **error reporting**  | yes                  | yes             | yes                |
| **GitHub additions** | ~10K                 | ~500K           | ~6K                |

[1]: https://github.com/Geal/nom
[2]: https://github.com/nikomatsakis/lalrpop
