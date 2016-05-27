# pest. Smart PEGs in Rust
[![Build Status](https://travis-ci.org/dragostis/pest.svg?branch=master)]
(https://travis-ci.org/dragostis/pest)
[![Coverage Status]
(https://coveralls.io/repos/github/dragostis/pest/badge.svg?branch=master)]
(https://coveralls.io/github/dragostis/pest?branch=master)
[![Cargo Crate](http://meritbadge.herokuapp.com/pest)]
(https://crates.io/crates/pest)

pest is a parser generator that works with
[PEGs](https://en.wikipedia.org/wiki/Parsing_expression_grammar).

It relies exclusively on macros to create an efficient parser at compile-time.

*The parser generator is currently experimental and will change.*

## Example
```rust
impl_rdp! {
    grammar! {
        exp = { paren ~ exp | [""] }
        paren = { ["("] ~ exp ~ [")"] }
    }
}

let mut parser = Rdp::new(StringInput::new("(()((())())()")));

assert!(parser.exp());
assert!(parser.end());
```

## Features

- [x] infix PEG rules
- [x] macro-generated recursive descent parser
- [x] white-space detection
- [x] token generation
- [x] automatic error reporting
- [ ] macro-generated Packrat parser
- [ ] large file buffer
- [ ] token pattern-matching
