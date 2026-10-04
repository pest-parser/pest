// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Detailed error reports (`set_error_detail(true)`) for parses that fail after a positive
//! lookahead succeeded. In its own test binary because `set_error_detail` is global.

use pest_meta::parser::Rule;
use pest_meta::{optimizer, parser};
use pest_vm::Vm;

/// The detailed error's furthest position and expected tokens (as their `Display`).
fn attempts(grammar: &str, rule: &str, input: &str) -> (usize, Vec<String>) {
    let pairs = parser::parse(Rule::grammar_rules, grammar).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    let vm = Vm::new(optimizer::optimize(ast));
    pest::set_error_detail(true);
    let error = vm.parse(rule, input).unwrap_err();
    let attempts = error.parse_attempts().unwrap();
    let expected = attempts
        .expected_tokens()
        .iter()
        .map(|token| token.to_string())
        .collect();
    (attempts.max_position, expected)
}

#[test]
fn successful_lookahead_does_not_hide_the_failure_after_it() {
    // The lookahead matches "ab"; the parse then fails at 0, expecting "x".
    assert_eq!(
        attempts(r#"r = { &"ab" ~ "x" }"#, "r", "abz"),
        (0, vec!["x".to_owned()])
    );
}

#[test]
fn failures_inside_a_successful_lookahead_are_not_reported() {
    // The lookahead ends its `+` at 4 by failing to match a hex digit there; the parse fails
    // at 2, expecting a decimal digit.
    assert_eq!(
        attempts(
            r#"num = { &("0x" ~ ASCII_HEX_DIGIT+) ~ "0x" ~ ASCII_DIGIT+ ~ EOI }"#,
            "num",
            "0xab"
        ),
        (2, vec!["0..9".to_owned()])
    );
}

#[test]
fn failed_lookahead_is_still_reported() {
    // The lookahead itself fails at 2: that is where the parse stops.
    assert_eq!(
        attempts(r#"r = { &("ab" ~ "c") ~ ANY* }"#, "r", "abz"),
        (2, vec!["c".to_owned()])
    );
}

#[test]
fn lookahead_covering_what_follows_is_unchanged() {
    // The failure after the lookahead is further than anything inside it.
    assert_eq!(
        attempts(
            r#"decl = { &(ASCII_ALPHA+ ~ "=") ~ ASCII_ALPHA+ ~ "=" ~ ASCII_DIGIT+ }"#,
            "decl",
            "ab=x"
        ),
        (3, vec!["0..9".to_owned()])
    );
}

#[test]
fn tokens_expected_before_a_successful_lookahead_are_kept() {
    // `"b"?` fails at 0 before the lookahead; the parse then fails at 0 on "d".
    assert_eq!(
        attempts(r#"r = { "b"? ~ &"c" ~ "d" }"#, "r", "cx"),
        (0, vec!["b".to_owned(), "d".to_owned()])
    );
}

#[test]
fn tokens_expected_before_a_failed_lookahead_are_kept() {
    // `"b"?` fails at 0, then the lookahead fails at 0 on "c": both are expected there.
    assert_eq!(
        attempts(r#"r = { "b"? ~ &"c" }"#, "r", "x"),
        (0, vec!["b".to_owned(), "c".to_owned()])
    );
}
