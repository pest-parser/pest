<p align="center">
  <img src="https://cdn.rawgit.com/dragostis/pest/9605855a9c4e86d5e89bc963fe50ee344d8a40e6/pest-logo.svg"/>
</p>
# pest. Elegant, efficient grammars

[![Join the chat at https://gitter.im/dragostis/pest](https://badges.gitter.im/dragostis/pest.svg)](https://gitter.im/dragostis/pest?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/dragostis/pest.svg?branch=master)]
(https://travis-ci.org/dragostis/pest)
[![Coverage Status]
(https://coveralls.io/repos/github/dragostis/pest/badge.svg?branch=master)]
(https://coveralls.io/github/dragostis/pest?branch=master)
[![Cargo Crate](http://meritbadge.herokuapp.com/pest)]
(https://crates.io/crates/pest)

pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser
generator with *simplicity* and *speed* in mind.

## [Documentation](http://dragostis.github.io/pest/pest)

## Elegant

pest uses PEG syntax to enable expressive grammar creation.

```rust
impl_rdp! {
    grammar! {
      expression = _{ paren ~ expression? }
      paren      =  { ["("] ~ expression? ~ [")"] }
    }
}

let mut parser = Rdp::new(StringInput::new("(()((())())()")));

assert!(parser.expression());
assert!(parser.end());
```

## Fast

pest generates a fast parser at compile time through the use of macros, without
forcing you to use nightly. Yes, it works on **stable** (1.9.0+).

| Parser generator | Time to parse 272.5 KB of JSON | pest speedup |
|------------------|--------------------------------|-------------:|
| ANTRL 4          | 153,000 μs (+/- 15,000)        |       48.12x |
| Bison + Flex     | 8,761.9 μs (+/- 697)           |        2.76x |
| **pest**         | 3,178.9 μs (+/- 40.6)          |        1.00x |

Tests have been run on an Intel Q8200, 4GB DDR2, Linux 4.6.2 as follows:

* ANTLR 4 [JSON grammar]
  (https://github.com/antlr/grammars-v4/blob/master/json/JSON.g4) measured with
  [JMH](http://openjdk.java.net/projects/code-tools/jmh/) SingleShotTime
* Bison + Flex [JSON grammar](https://gist.github.com/justjkk/436828) with
  string capturing and printing removed
* pest [JSON grammar](benches/json.rs) measured with `cargo bench`

## Features

* simple PEG grammar
* smart error reporting with `atomic` and `silent` rules
* precedence climbing rule
* no generation step (one-step compilation)
* fast macro-generated recursive descent parser
* runs on stable Rust
* elegant functional-style `Token` processing

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

## Roadmap

* [Packrat parsing](issues/7)
* [parsing parallelization](issues/25)
