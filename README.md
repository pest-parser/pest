<p align="center">
  <img src="https://cdn.rawgit.com/pest-parser/pest/3279f28f/pest-logo.svg" width="80%"/>
</p>

# pest. The Elegant Parser <sup>(beta)</sup>

[![Join the chat at https://gitter.im/dragostis/pest](https://badges.gitter.im/dragostis/pest.svg)](https://gitter.im/dragostis/pest?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/pest-parser/pest.svg?branch=master)](https://travis-ci.org/pest-parser/pest)
[![codecov](https://codecov.io/gh/pest-parser/pest/branch/master/graph/badge.svg)](https://codecov.io/gh/pest-parser/pest)
[![Crates.io](https://img.shields.io/crates/v/pest.svg)](https://crates.io/crates/pest)
[![Crates.io](https://img.shields.io/crates/d/pest.svg)](https://crates.io/crates/pest)
[![Docs](https://docs.rs/pest/badge.svg)](https://docs.rs/pest)

pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser with *simplicity* and *speed* in mind.

## Elegant grammar

Defining a grammar for an alpha-numeric identifier that does not start with a digit is a straight-forward as:

```
alpha = { 'a'..'z' | 'A'..'Z' }
digit = { '0'..'9' }

ident = _{ !digit ~ (alpha | digit)+ }
     // ^
     // ident rule is silent which means it produces no tokens
```

This is then saved in a `.pest` grammar file and is never mixed up with Rust code which results in an always up-to-date
formal definition of the grammar which is very easy to maintain.

## Pairs API

The grammar can be used to derive a `Parser` implementation automatically. Parsing returns nested token pairs that can
be simply iterated over in order to print out letters and digits:

```rust
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "ident.pest"]
struct IdentParser;

fn main() {
    let pairs = IdentParser::parse_str(Rule::ident, "abc123").unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        match pair.as_rule() {
            Rule::alpha => println!("letter: {}", pair.into_span().as_str()),
            Rule::digit => println!("digit: {}", pair.into_span().as_str()),
            _ => unreachable!() // ident rule is silent and cannot be reached
        };
    }
}
```

## Meaningful error reporting

Parsing `"123"` instead of `"abc123"` in the code above will result in the following panic:

```
thread 'main' panicked at ' --> 1:1
  |
1 | 123
  | ^---
  |
  = unexpected digit', src/main.rs:12:78
```

## Other features

* fast macro-generated recursive descent parser
* precedence climbing
* input handling
* custom errors
* runs on stable Rust

## Usage
pest requires [Cargo and Rust](https://www.rust-lang.org/en-US/downloads.html).

Add the following to `Cargo.toml`:

```toml
pest = "^1.0.0-beta"
pest_derive = "^1.0.0-beta"
```

and in your Rust `lib.rs` or `main.rs`:

```rust
extern crate pest;
#[macro_use]
extern crate pest_derive;
```

## Projects using pest

* [brain](https://github.com/brain-lang/brain)
* [handlebars-rust](https://github.com/sunng87/handlebars-rust)
* [pest](https://github.com/pest-parser/pest/blob/master/pest_derive/src/parser.rs) (bootstrapped)
* [rouler](https://github.com/jarcane/rouler)
* [stache](https://github.com/dgraham/stache)
* [tera](https://github.com/Keats/tera)
* [ukhasnet-parser](https://github.com/adamgreig/ukhasnet-parser)
