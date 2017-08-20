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

Defining a grammar for a list of alpha-numeric identifiers that do not start with a digit is a straight-forward as:

```rust
alpha = { 'a'..'z' | 'A'..'Z' }
digit = { '0'..'9' }

ident = { !digit ~ (alpha | digit)+ }

ident_list = _{ ident ~ ( " " ~ ident )+ }
          // ^
          // ident_list rule is silent which means it produces no tokens
```

This is then saved in a `.pest` grammar file and is never mixed up with Rust code which results in an always up-to-date
formal definition of the grammar which is very easy to maintain.

## Pairs API

The grammar can be used to derive a `Parser` implementation automatically. Parsing returns an iterator of nested token pairs:

```rust
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "ident.pest"]
struct IdentParser;

fn main() {
    let pairs = IdentParser::parse_str(Rule::ident_list, "a1 b2").unwrap_or_else(|e| panic!("{}", e));

    // Because ident_list is silent, the iterator will contain idents
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule: {:?}", pair.as_rule());
        println!("Span: {:?}", pair.clone().into_span());
        println!("Text: {}", pair.clone().into_span().as_str());

        // A pair can be converted to an iterator of the tokens which make it up:
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::alpha => println!("letter: {}", inner_pair.into_span().as_str()),
                Rule::digit => println!("digit: {}", inner_pair.into_span().as_str()), 
                _ => unreachable!()
            };
        }
    }
}
```

This produces the following output:
```
Rule: ident
Span: Span { start: 0, end: 2 }
Text: a1
letter: a
digit: 1
Rule: ident
Span: Span { start: 3, end: 5 }
Text: b2
letter: b
digit: 2
```

## Meaningful error reporting

Parsing `"123"` instead of `"a1 b2"` in the code above will result in the following panic:

```
thread 'main' panicked at ' --> 1:1
  |
1 | 123
  | ^---
  |
  = expected ident', src\main.rs:12
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
* [graphql-parser](https://github.com/Keats/graphql-parser)
* [handlebars-rust](https://github.com/sunng87/handlebars-rust)
* [pest](https://github.com/pest-parser/pest/blob/master/pest_derive/src/parser.rs) (bootstrapped)
* [rouler](https://github.com/jarcane/rouler)
* [rs_pbrt](https://github.com/wahn/rs_pbrt)
* [stache](https://github.com/dgraham/stache)
* [tera](https://github.com/Keats/tera)
* [ukhasnet-parser](https://github.com/adamgreig/ukhasnet-parser)
