<p align="center">
  <img src="https://raw.github.com/pest-parser/pest/master/pest-logo.svg?sanitize=true" width="80%"/>
</p>

# pest. The Elegant Parser

[![Join the chat at https://gitter.im/dragostis/pest](https://badges.gitter.im/dragostis/pest.svg)](https://gitter.im/dragostis/pest?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Book](https://img.shields.io/badge/book-WIP-4d76ae.svg)](https://pest-parser.github.io/book)
[![Docs](https://docs.rs/pest/badge.svg)](https://docs.rs/pest)

[![Build Status](https://travis-ci.org/pest-parser/pest.svg?branch=master)](https://travis-ci.org/pest-parser/pest)
[![codecov](https://codecov.io/gh/pest-parser/pest/branch/master/graph/badge.svg)](https://codecov.io/gh/pest-parser/pest)
[![Fuzzit Status](https://app.fuzzit.dev/badge?org_id=pest-parser)](https://app.fuzzit.dev/orgs/pest-parser/dashboard)
[![Crates.io](https://img.shields.io/crates/d/pest.svg)](https://crates.io/crates/pest)
[![Crates.io](https://img.shields.io/crates/v/pest.svg)](https://crates.io/crates/pest)

pest is a general purpose parser written in Rust with a focus on accessibility,
correctness, and performance. It uses parsing expression grammars
(or [PEG]) as input, which are similar in spirit to regular expressions, but
which offer the enhanced expressivity needed to parse complex languages.

[PEG]: https://en.wikipedia.org/wiki/Parsing_expression_grammar

## Getting started

The recommended way to start parsing with pest is to read the official [book].

Other helpful resources:

* API reference on [docs.rs]
* play with grammars and share them on our [fiddle]
* leave feedback, ask questions, or greet us on [Gitter]

[book]: https://pest-parser.github.io/book
[docs.rs]: https://docs.rs/pest
[fiddle]: https://pest-parser.github.io/#editor
[Gitter]: https://gitter.im/dragostis/pest

## Example

The following is an example of a grammar for a list of alpha-numeric identifiers
where the first identifier does not start with a digit:

```rust
alpha = { 'a'..'z' | 'A'..'Z' }
digit = { '0'..'9' }

ident = { (alpha | digit)+ }

ident_list = _{ !digit ~ ident ~ (" " ~ ident)+ }
          // ^
          // ident_list rule is silent which means it produces no tokens
```

Grammars are saved in separate .pest files which are never mixed with procedural
code. This results in an always up-to-date formalization of a language that is
easy to read and maintain.

## Meaningful error reporting

Based on the grammar definition, the parser also includes automatic error
reporting. For the example above, the input `"123"` will result in:

```
thread 'main' panicked at ' --> 1:1
  |
1 | 123
  | ^---
  |
  = unexpected digit', src/main.rs:12
```
while `"ab *"` will result in:
```
thread 'main' panicked at ' --> 1:1
  |
1 | ab *
  |    ^---
  |
  = expected ident', src/main.rs:12
```

## Pairs API

The grammar can be used to derive a `Parser` implementation automatically.
Parsing returns an iterator of nested token pairs:

```rust
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "ident.pest"]
struct IdentParser;

fn main() {
    let pairs = IdentParser::parse(Rule::ident_list, "a1 b2").unwrap_or_else(|e| panic!("{}", e));

    // Because ident_list is silent, the iterator will contain idents
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());

        // A pair can be converted to an iterator of the tokens which make it up:
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::alpha => println!("Letter:  {}", inner_pair.as_str()),
                Rule::digit => println!("Digit:   {}", inner_pair.as_str()),
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

## Other features

* Precedence climbing
* Input handling
* Custom errors
* Runs on stable Rust

## Projects using pest

* [pest_meta](https://github.com/pest-parser/pest/blob/master/meta/src/grammar.pest) (bootstrapped)
* [AshPaper](https://github.com/shnewto/ashpaper)
* [brain](https://github.com/brain-lang/brain)
* [Chelone](https://github.com/Aaronepower/chelone)
* [comrak](https://github.com/kivikakk/comrak)
* [elastic-rs](https://github.com/cch123/elastic-rs)
* [graphql-parser](https://github.com/Keats/graphql-parser)
* [handlebars-rust](https://github.com/sunng87/handlebars-rust)
* [hexdino](https://github.com/Luz/hexdino)
* [Huia](https://gitlab.com/jimsy/huia/)
* [jql](https://github.com/yamafaktory/jql)
* [json5-rs](https://github.com/callum-oakley/json5-rs)
* [mt940](https://github.com/svenstaro/mt940-rs)
* [py_literal](https://github.com/jturner314/py_literal)
* [rouler](https://github.com/jarcane/rouler)
* [RuSh](https://github.com/lwandrebeck/RuSh)
* [rs_pbrt](https://github.com/wahn/rs_pbrt)
* [stache](https://github.com/dgraham/stache)
* [tera](https://github.com/Keats/tera)
* [ui_gen](https://github.com/emoon/ui_gen)
* [ukhasnet-parser](https://github.com/adamgreig/ukhasnet-parser)
* [ZoKrates](https://github.com/ZoKrates/ZoKrates)

## Special thanks

A special round of applause goes to prof. Marius Minea for his guidance and all
pest contributors, some of which being none other than my friends.
