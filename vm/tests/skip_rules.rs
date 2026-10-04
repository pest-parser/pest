// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! WHITESPACE and COMMENT with the `!` and `$` modifiers parse as pest_derive's generated code
//! parses them: the rule produces a pair, and its body is atomic. Expected tokens and errors
//! are what pest_derive gives for the same grammars.

#[macro_use]
extern crate pest_vm;

use pest_meta::parser::Rule;
use pest_meta::{optimizer, parser};
use pest_vm::Vm;

fn vm(grammar: &str) -> Vm {
    let pairs = parser::parse(Rule::grammar_rules, grammar).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    Vm::new(optimizer::optimize(ast))
}

const NON_ATOMIC: &str = r##"
WHITESPACE = !{ " " | "\n" }
COMMENT = !{ "#" ~ (!"\n" ~ ANY)* }
pair = { word ~ "=" ~ word }
word = @{ ASCII_ALPHA+ }
"##;

#[test]
fn non_atomic_comment_produces_a_pair() {
    parses_to! {
        parser: vm(NON_ATOMIC),
        input: "#lead",
        rule: "COMMENT",
        tokens: [
            COMMENT(0, 5)
        ]
    };
}

#[test]
fn non_atomic_comment_reports_itself() {
    fails_with! {
        parser: vm(NON_ATOMIC),
        input: "b",
        rule: "COMMENT",
        positives: vec!["COMMENT"],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn non_atomic_skip_rules_between_tokens() {
    parses_to! {
        parser: vm(NON_ATOMIC),
        input: "a #c\n= b",
        rule: "pair",
        tokens: [
            pair(0, 8, [
                word(0, 1),
                WHITESPACE(1, 2),
                COMMENT(2, 4),
                WHITESPACE(4, 5),
                WHITESPACE(6, 7),
                word(7, 8)
            ])
        ]
    };
}

const BODY_ATOMIC: &str = r##"
WHITESPACE = { " " }
COMMENT = !{ "#" ~ "c" }
r = { "a" ~ "b" }
"##;

#[test]
fn non_atomic_comment_between_tokens() {
    parses_to! {
        parser: vm(BODY_ATOMIC),
        input: "a #c b",
        rule: "r",
        tokens: [
            r(0, 6, [
                WHITESPACE(1, 2),
                COMMENT(2, 4),
                WHITESPACE(4, 5)
            ])
        ]
    };
}

// The body of a non-atomic COMMENT is still atomic: no WHITESPACE between `#` and `c`.
#[test]
fn non_atomic_comment_body_is_atomic() {
    fails_with! {
        parser: vm(BODY_ATOMIC),
        input: "a # c b",
        rule: "r",
        positives: vec!["COMMENT", "WHITESPACE"],
        negatives: vec![],
        pos: 2
    };
}

// `$` (compound atomic) was already parsed this way; kept here next to `!`.
#[test]
fn compound_atomic_comment_produces_a_pair() {
    parses_to! {
        parser: vm(r##"
COMMENT = ${ "#" ~ "x" }
r = { "a" ~ "b" }
"##),
        input: "a#xb",
        rule: "r",
        tokens: [
            r(0, 4, [
                COMMENT(1, 3)
            ])
        ]
    };
}
