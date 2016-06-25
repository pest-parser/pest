// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! # pest. Elegant, efficient grammars
//!
//! pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser generator with
//! *simplicity* and *speed* in mind.
//!
//! ## Input & Parser
//!
//! pest works mainly through two `trait`s: `Input` & `Parser`. `Input` is used to remember the
//! current position or index of the to-be-parsed input. It also knows how to match a `&str` or
//! `char` range against the current position. If it did in fact match, it advances the input by
//! incrementing the position.
//!
//! ```
//! # use pest::Input;
//! # use pest::StringInput;
//! let mut input = StringInput::new("asdasdf"); // Input mutates its position
//!
//! assert_eq!(input.pos(), 0);                  // matching starts from 0, before 'a'
//!
//! assert!(input.match_string("asd"));          // Input::match_string matches "asd"; returns true
//!
//! assert_eq!(input.pos(), 3);                  // last match advances the parser by "asd".len()
//!
//! assert!(input.match_range('a', 'z'));        // Input::match_range matches 'a'; returns true
//!
//! assert_eq!(input.pos(), 4);                  // last match advances the parser by 1
//! ```
//!
//! `Input` is also supposed to return a `&str` `slice` of its input by calling
//! [`Input::slice`](trait.Input#tymethod.slice).
//!
//! `Parser` gets constructed on top of an `Input` and delegates position access to
//! [`Parser::pos`](trait.Parser#tymethod.pos) and
//! [`Parser::set_pos`](trait.Parser#tymethod.set_pos). Apart from this, `Parser` also gives access
//! to its `Token` queue and expected rules to match when it fails.
//!
//! ## grammar!
//!
//! The [`grammar!`](macro.grammar!) `macro` processes every rule and generates a method on a
//! `Parser` that returns whether the rule has matched its `Input`. `grammar!` can only be used
//! inside of [`impl_rdp!`](macro.impl_rdp!) right now until other parser algorithms are
//! implemented.
//!
//! *Note:* `grammar!` may require you to increase the recursion limit of your create with
//! `#![recursion_limit = "*"]` where * is the new limit.
//!
//! When `impl_rdp!` is run, it implements an `enum` called `Rule` that has a value for all
//! [non-silent](macro.grammar!#silent-rules-_) rules, but also for
//! [`any` and `eoi`](macro.grammar!). These `Rule`s are used within `Token`s to specify the type
//! of rule that matched. These `Tokens` are accesible from
//! [`Parser::queue`](trait.Parser#tymethod.queue) after parsing. Instead of having the shape of
//! an AST, the `Token`s come in a `Vec` in a predefined order that makes them easy to process.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{ paren ~ expression? } // expression is silent so we'll only have parens
//!         paren      =  { ["("] ~ expression? ~ [")"] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("(())()"));
//!                                         //  ^--^   - Token { paren, 0, 4 }; queue[0]
//!                                         //   ^^    - Token { paren, 1, 3 }; queue[1]
//!                                         //      ^^ - Token { paren, 4, 6 }; queue[2]
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//!
//! let queue = vec![
//!     Token::new(Rule::paren, 0, 4),
//!     Token::new(Rule::paren, 1, 3),
//!     Token::new(Rule::paren, 4, 6)
//! ];
//!
//! assert_eq!(parser.queue(), &queue);
//! # }
//! ```
//!
//! `Rule`s are also used for error reporting through
//! [`Parser::queue`](trait.Parser#tymethod.queue) which is used when a `Parser` failed to parse
//! and you want to see what `Rule`s it expected at the last possible position.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{ paren ~ expression? } // expression is silent so we'll only have parens
//!         paren      =  { ["("] ~ expression? ~ [")"] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("(())()foo"));
//!                                         //        ^   - Parser should expect a paren at pos 6
//!
//! assert!(parser.expression()); // the parser goes as deep as it can
//! assert!(!parser.end());       // end is not reached, so the whole Input was not matched
//!
//! assert_eq!(parser.expected(), (vec![Rule::paren], 6));
//! # }
//! ```
//!
//! *Note:* You can use the `eoi` rule instead of calling
//! [`Parser::end`](trait.Parser#tymethod.end) manually.
//!
//! # Calculator example
//!
//! This example will concentrate on parsing and solving simple airthmetic with parens, additions,
//! subtractions, multiplications, and divisions.
//!
//! Let's start with defining a rule that matches integers. We first need to match an optional
//! `"-"`.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         number = { ["-"]? }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("-"));
//!
//! assert!(parser.number());
//! assert!(parser.end());
//! # }
//! ```
//!
//! In order to match a number, we should deal with two cases:
//!
//! * number is `0`
//! * number is any other number *not* starting with `0`
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!                //  |    | |  |     | |          | |         ^ zero or more
//!                //  |    | |  |     | |          | ^ digit
//!                //  |    | |  |     | |          ^ followed by
//!                //  |    | |  |     | ^ non-zero digit
//!                //  |    | |  |     ^ or
//!                //  |    | |  ^ zero
//!                //  |    | ^ followed by
//!                //  |    ^ optional
//!                //  ^ minus
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("-90"));
//!
//! assert!(parser.number());
//! assert!(parser.end());
//! # }
//! ```
//!
//! Now let's add operator rules.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   = { ["+"] }
//!         minus  = { ["-"] }
//!         times  = { ["*"] }
//!         slash  = { ["/"] }
//!     }
//! }
//! #
//! # let mut parser = Rdp::new(StringInput::new("-90"));
//! #
//! # assert!(parser.number());
//! # assert!(parser.end());
//! # }
//! ```
//!
//! Because infix precedence is hard to implement in PEG and quite inefficient, pest comes with a
//! [rule](macro.grammar!#precedence-climbing) that implements [precedence climbing]
//! (https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method).
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = {
//!             { number }                         // primary rule is a number
//!             addition       = { plus  | minus } // precedence 0 is addition
//!             multiplication = { times | slash } // precedence 1 is multiplication
//!         }
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   = { ["+"] }
//!         minus  = { ["-"] }
//!         times  = { ["*"] }
//!         slash  = { ["/"] }
//!     }
//! }
//! #
//! # let mut parser = Rdp::new(StringInput::new("-90"));
//! #
//! # assert!(parser.number());
//! # assert!(parser.end());
//! # }
//! ```
//!
//! Before we go any further, let's see what parsing a `number` from an `expression` places on to
//! the queue.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! # impl_rdp! {
//! #     grammar! {
//! #         expression = {
//! #             { number }                         // primary rule is a number
//! #             addition       = { plus  | minus } // precedence 0 is addition
//! #             multiplication = { times | slash } // precedence 1 is multiplication
//! #         }
//! #         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//! #         plus   = { ["+"] }
//! #         minus  = { ["-"] }
//! #         times  = { ["*"] }
//! #         slash  = { ["/"] }
//! #     }
//! # }
//! #
//! let mut parser = Rdp::new(StringInput::new("-90"));
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//!
//! println!("{:?}", parser.queue()); // [Token { rule: expression, start: 0, end: 3 },
//!                                   //  Token { rule: number, start: 0, end: 3 }]
//! # }
//! ```
//!
//! Since we're already parsing an `expression` and don't care about its length, we can make it
//! [silent](macro.grammar!#silent-rules-_).
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{ // the underscore tells pest that this rule is silent
//!             { number }
//!             addition       = { plus  | minus }
//!             multiplication = { times | slash }
//!         }
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   = { ["+"] }
//!         minus  = { ["-"] }
//!         times  = { ["*"] }
//!         slash  = { ["/"] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("-90"));
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//!
//! let queue = vec![
//!     Token::new(Rule::number, 0, 3)
//! ];
//!
//! assert_eq!(parser.queue(), &queue);
//! # }
//! ```
//!
//! Adding parens to the whole business is as easy as adding a paren rule to the primary rule of
//! the precedence climber.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{
//!             { ["("] ~ expression ~ [")"] | number }
//!             addition       = { plus  | minus }
//!             multiplication = { times | slash }
//!         }
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   = { ["+"] }
//!         minus  = { ["-"] }
//!         times  = { ["*"] }
//!         slash  = { ["/"] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("((-90))"));
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//!
//! let queue = vec![
//!     Token::new(Rule::number, 2, 5)
//! ];
//!
//! assert_eq!(parser.queue(), &queue);
//! # }
//! ```
//!
//! Before we get to the processing of the `Token`s, let's also add white-space to the grammar.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{
//!             { ["("] ~ expression ~ [")"] | number }
//!             addition       = { plus  | minus }
//!             multiplication = { times | slash }
//!         }
//!         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   = { ["+"] }
//!         minus  = { ["-"] }
//!         times  = { ["*"] }
//!         slash  = { ["/"] }
//!
//!         whitespace = _{ [" "] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("2 + 2"));
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//! # }
//! ```
//!
//! But now trying to parse `"9 9"` will work and recognize it as a `number`.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! # impl_rdp! {
//! #     grammar! {
//! #         expression = _{
//! #             { ["("] ~ expression ~ [")"] | number }
//! #             addition       = { plus  | minus }
//! #             multiplication = { times | slash }
//! #         }
//! #         number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//! #         plus   = { ["+"] }
//! #         minus  = { ["-"] }
//! #         times  = { ["*"] }
//! #         slash  = { ["/"] }
//! #
//! #         whitespace = _{ [" "] }
//! #     }
//! # }
//! #
//! let mut parser = Rdp::new(StringInput::new("9 9"));
//!
//! assert!(parser.expression());
//! assert!(parser.end());
//!
//! let queue = vec![
//!     Token::new(Rule::number, 0, 3)
//! ];
//!
//! assert_eq!(parser.queue(), &queue);
//! # }
//! ```
//!
//! To solve this issue, we make `number` [atomic](macro.grammar!#atomic-rules-), stopping any
//! white-space matching inside of the rule.
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{
//!             { ["("] ~ expression ~ [")"] | number }
//!             addition       = { plus  | minus }
//!             multiplication = { times | slash }
//!         }
//!         number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   =  { ["+"] }
//!         minus  =  { ["-"] }
//!         times  =  { ["*"] }
//!         slash  =  { ["/"] }
//!
//!         whitespace = _{ [" "] }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("9 9"));
//!
//! assert!(parser.expression());
//! assert!(!parser.end());
//! # }
//! ```
//!
//! To process all these `Token`s we'll use the [`process!`](macro.process!) `macro`. This `macro`
//! defines matcher methods on the `Parser` that pattern match against a set of `Token`s from the
//! front of the queue, calling themselves recursively until everything matched and returned its
//! result.
//!
//! Let's start by defining the signature of the `compute` matcher that will do the computation. We
//! need it to return an `i32` in the end.
//!
//! ```ignore
//! compute(&self) -> i32
//! ```
//!
//! Now all we need to do is to write the three cases of interest, namely `number`, `addition`, and
//! `multiplication`. `number` is captured (its `&str` is being sliced from the `Input`) with the
//! `&` pattern and then parsed to an `i32`.
//!
//! ```ignore
//! (&number: number) => number.parse::<i32>().unwrap()
//! ```
//!
//! `addition` and `multiplication` are virtually identical:
//!
//! * match the `addition`/`multiplication` `Token` without using it
//! * recursively process the left-hand-side by calling `compute`
//! * use the `sign` `Token` without capturing its `&str` value
//! * recursively process the right-hand-side by calling `compute`
//! * match `sign` inside the block and return the appropriate result
//!
//! ```ignore
//! (_: addition, left: compute(), sign, right: compute()) => {
//!     match sign.rule {
//!         Rule::plus  => left + right,
//!         Rule::minus => left - right,
//!         _ => unreachable!()
//!     }
//! },
//! (_: multiplication, left: compute(), sign, right: compute()) => {
//!     match sign.rule {
//!         Rule::times => left * right,
//!         Rule::slash => left / right,
//!         _ => unreachable!()
//!     }
//! }
//! ```
//!
//! The reason we're matching `sign` manually inside of the block is because using `_: plus` and
//! `_: minus` will cause `left: main()` to be run twice in case the first rule fails. Caching the
//! result in this case is non-trivial apart from the fact that duplicate complex patterns are not
//! necessarily easier to read.
//!
//! Now for the whole example:
//!
//! ```
//! # #[macro_use] extern crate pest;
//! # use pest::Parser;
//! # use pest::Token;
//! # use pest::Input;
//! # use pest::StringInput;
//! # fn main() {
//! impl_rdp! {
//!     grammar! {
//!         expression = _{
//!             { ["("] ~ expression ~ [")"] | number }
//!             addition       = { plus  | minus }
//!             multiplication = { times | slash }
//!         }
//!         number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
//!         plus   =  { ["+"] }
//!         minus  =  { ["-"] }
//!         times  =  { ["*"] }
//!         slash  =  { ["/"] }
//!
//!         whitespace = _{ [" "] }
//!     }
//!
//!     process! {
//!         compute(&self) -> i32 {
//!             (&number: number) => number.parse::<i32>().unwrap(),
//!             (_: addition, left: compute(), sign, right: compute()) => {
//!                 match sign.rule {
//!                     Rule::plus  => left + right,
//!                     Rule::minus => left - right,
//!                     _ => unreachable!()
//!                 }
//!             },
//!             (_: multiplication, left: compute(), sign, right: compute()) => {
//!                 match sign.rule {
//!                     Rule::times => left * right,
//!                     Rule::slash => left / right,
//!                     _ => unreachable!()
//!                 }
//!             }
//!         }
//!     }
//! }
//!
//! let mut parser = Rdp::new(StringInput::new("(3 + (9 + 3 * 4 + (3 + 1) / 2 - 4)) * 2"));
//!
//! assert!(parser.expression());
//! assert_eq!(parser.compute(), 44);
//! # }
//! ```

#[macro_use]
mod grammar;
#[macro_use]
mod process;
#[macro_use]
mod parsers;
mod input;
mod inputs;
mod parser;

pub mod prelude;

pub use input::Input;
pub use inputs::StringInput;
pub use parser::Parser;
pub use parsers::Token;
