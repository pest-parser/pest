// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use pest::Parser;
use pest::Token;
use pest::Input;
use pest::StringInput;

impl_rdp! {
    grammar! {
        exp = _{ paren ~ exp | [""] }
        paren = { ["("] ~ exp ~ [")"] }
        rep_zero = { ["a"]* ~ eoi }
        rep_one = { ["a"]+ }
        opt = { ["a"]? }
        pres = { &["a"] }
        abs = { !(["a"] | ["b"]) ~ any }
        digit = { ['0'..'9'] }
        number = { ['0'..'9']+ }
        plus = { ["+"] }
        times = { ["*"] }
        power = { ["^"] }

        expression = {
            { number }
            add = _{ plus }
            mul = { times }
            pow = {< power }
        }

        whitespace = _{ [" "] }
    }
}

#[test]
fn basic() {
    let mut parser = Rdp::new(StringInput::new("(())((())())()"));

    assert!(parser.exp());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::paren, start: 0, end: 4 },
        Token { rule: Rule::paren, start: 1, end: 3 },
        Token { rule: Rule::paren, start: 4, end: 12 },
        Token { rule: Rule::paren, start: 5, end: 9 },
        Token { rule: Rule::paren, start: 6, end: 8 },
        Token { rule: Rule::paren, start: 9, end: 11 },
        Token { rule: Rule::paren, start: 12, end: 14 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn fail() {
    let mut parser = Rdp::new(StringInput::new("(())((())())("));

    assert!(parser.exp());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::paren, start: 0, end: 4 },
        Token { rule: Rule::paren, start: 1, end: 3 },
        Token { rule: Rule::paren, start: 4, end: 12 },
        Token { rule: Rule::paren, start: 5, end: 9 },
        Token { rule: Rule::paren, start: 6, end: 8 },
        Token { rule: Rule::paren, start: 9, end: 11 }
    ];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::paren], 13));
}

#[test]
fn rep_zero_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(parser.rep_zero());

    let queue = vec![
        Token { rule: Rule::rep_zero, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_zero_long() {
    let mut parser = Rdp::new(StringInput::new("aaaa"));

    assert!(parser.rep_zero());

    let queue = vec![
        Token { rule: Rule::rep_zero, start: 0, end: 4 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_zero_wrong() {
    let mut parser = Rdp::new(StringInput::new("aaaab"));

    assert!(!parser.rep_zero());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::eoi], 4));
}

#[test]
fn rep_one_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(!parser.rep_one());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn rep_one_long() {
    let mut parser = Rdp::new(StringInput::new("aaaa"));

    assert!(parser.rep_one());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::rep_one, start: 0, end: 4 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_one_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(!parser.rep_one());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn opt_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn opt_right() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn opt_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(parser.opt());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pres_right() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(parser.pres());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::pres, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pres_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(!parser.pres());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::pres], 0));
}

#[test]
fn abs_right() {
    let mut parser = Rdp::new(StringInput::new("c"));

    assert!(parser.abs());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::abs, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn abs_wrong() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(!parser.abs());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::abs], 0));
}

#[test]
fn digit_right() {
    let mut parser = Rdp::new(StringInput::new("0"));

    assert!(parser.digit());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::digit, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn digit_wrong() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(!parser.digit());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::digit], 0));
}

#[test]
fn expression() {
    let mut parser = Rdp::new(StringInput::new("1+2+3*9^2^2+2"));

    assert!(parser.expression());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::expression, start: 0, end: 13 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::plus, start: 1, end: 2 },
        Token { rule: Rule::number, start: 2, end: 3 },
        Token { rule: Rule::plus, start: 3, end: 4 },
        Token { rule: Rule::mul, start: 4, end: 11 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::times, start: 5, end: 6 },
        Token { rule: Rule::pow, start: 6, end: 11 },
        Token { rule: Rule::number, start: 6, end: 7 },
        Token { rule: Rule::power, start: 7, end: 8 },
        Token { rule: Rule::pow, start: 8, end: 11 },
        Token { rule: Rule::number, start: 8, end: 9 },
        Token { rule: Rule::power, start: 9, end: 10 },
        Token { rule: Rule::number, start: 10, end: 11 },
        Token { rule: Rule::plus, start: 11, end: 12 },
        Token { rule: Rule::number, start: 12, end: 13 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn expression_spaced() {
    let mut parser = Rdp::new(StringInput::new("1 + 2 + 3 * 9^2^2 + 2"));

    assert!(parser.expression());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::expression, start: 0, end: 21 },
        Token { rule: Rule::number, start: 0, end: 1 },
        Token { rule: Rule::plus, start: 2, end: 3 },
        Token { rule: Rule::number, start: 4, end: 5 },
        Token { rule: Rule::plus, start: 6, end: 7 },
        Token { rule: Rule::mul, start: 8, end: 17 },
        Token { rule: Rule::number, start: 8, end: 9 },
        Token { rule: Rule::times, start: 10, end: 11 },
        Token { rule: Rule::pow, start: 12, end: 17 },
        Token { rule: Rule::number, start: 12, end: 13 },
        Token { rule: Rule::power, start: 13, end: 14 },
        Token { rule: Rule::pow, start: 14, end: 17 },
        Token { rule: Rule::number, start: 14, end: 15 },
        Token { rule: Rule::power, start: 15, end: 16 },
        Token { rule: Rule::number, start: 16, end: 17 },
        Token { rule: Rule::plus, start: 18, end: 19 },
        Token { rule: Rule::number, start: 20, end: 21 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn expression_space_after() {
    let mut parser = Rdp::new(StringInput::new("1 "));

    assert!(parser.expression());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::expression, start: 0, end: 1 },
        Token { rule: Rule::number, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}
