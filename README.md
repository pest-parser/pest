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

pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser with [simplicity][1] and [speed][2] in mind.

[1]: https://github.com/pest-parser/pest#elegant-grammar
[2]: https://github.com/pest-parser/pest#sheer-performance

## Elegant grammar

Defining a grammar for a list of alpha-numeric identifiers where the first identifier does not start with a digit is as
straight-forward as:

```rust
alpha = { 'a'..'z' | 'A'..'Z' }
digit = { '0'..'9' }

ident = { (alpha | digit)+ }

ident_list = _{ !digit ~ ident ~ (" " ~ ident)+ }
          // ^
          // ident_list rule is silent which means it produces no tokens
```

This is then saved in a `.pest` grammar file and is never mixed up with Rust code which results in an always up-to-date
formal definition of the grammar which is very easy to maintain.

## Pairs API

The grammar can be used to derive a `Parser` implementation automatically. Parsing returns an iterator of nested token
pairs:

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
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.clone().into_span());
        println!("Text:    {}", pair.clone().into_span().as_str());

        // A pair can be converted to an iterator of the tokens which make it up:
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::alpha => println!("Letter:  {}", inner_pair.into_span().as_str()),
                Rule::digit => println!("Digit:   {}", inner_pair.into_span().as_str()), 
                _ => unreachable!()
            };
        }
    }
}
```

This produces the following output:
```
Rule:    ident
Span:    Span { start: 0, end: 2 }
Text:    a1
Letter:  a
Digit:   1
Rule:    ident
Span:    Span { start: 3, end: 5 }
Text:    b2
Letter:  b
Digit:   2
```

## Meaningful error reporting

Parsing `"123"` instead of `"a1 b2"` in the code above will result in the following panic:

```
thread 'main' panicked at ' --> 1:1
  |
1 | 123
  | ^---
  |
  = unexpected digit', src/main.rs:12
```

while parsing `"ab *"` will result in:

```
thread 'main' panicked at ' --> 1:4
  |
1 | ab *
  |    ^---
  |
  = expected ident', src/main.rs:12
```

## Sheer performance

pest provides parsing performance in the same league as carefully written manual parsers.
The following JSON benchmark puts it somehwere in between one of the most optimized JSON parsers,
[ujson4c](https://github.com/esnme/ujson4c), and a static native-speed parser, [nom](https://github.com/Geal/nom).

The first entry of pest scores 22ms, while the second scores 78ms since it's mapping `Pair`s
to a custom JSON AST.

<p align="center">
  <img src="https://cdn.rawgit.com/pest-parser/pest/9ca9a111/results.svg"/>
</p>

The benchmark uses [a large 2MB JSON file](https://github.com/miloyip/nativejson-benchmark/blob/master/data/canada.json).
Tested on a Intel® Core™ i7-6700HQ running Linux.

Sources:

* [ujson4c](https://gist.github.com/dragostis/541363fee0075480f13c357e1604f343)
* [pest](https://github.com/pest-parser/benchmark)
* [pest (custom AST)](https://github.com/pest-parser/pest/blob/master/pest_grammars/benches/json.rs)
* [nom](https://github.com/Geal/nom/blob/master/benches/json.rs)

## Other features

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
* [RuSh](https://github.com/lwandrebeck/RuSh)
* [rs_pbrt](https://github.com/wahn/rs_pbrt)
* [stache](https://github.com/dgraham/stache)
* [tera](https://github.com/Keats/tera)
* [ukhasnet-parser](https://github.com/adamgreig/ukhasnet-parser)
