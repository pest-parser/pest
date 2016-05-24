// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use pest::Parser;
use pest::Queues;
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
    }
}

#[test]
fn basic() {
    let mut parser = Rdp::new(Box::new(StringInput::new("(())((())())()")));

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
    let mut parser = Rdp::new(Box::new(StringInput::new("(())((())())(")));

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
    let mut parser = Rdp::new(Box::new(StringInput::new("")));

    assert!(parser.rep_zero());

    let queue = vec![
        Token { rule: Rule::rep_zero, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_zero_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("aaaa")));

    assert!(parser.rep_zero());

    let queue = vec![
        Token { rule: Rule::rep_zero, start: 0, end: 4 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_zero_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("aaaab")));

    assert!(!parser.rep_zero());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::eoi], 4));
}

#[test]
fn rep_one_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("")));

    assert!(!parser.rep_one());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn rep_one_long() {
    let mut parser = Rdp::new(Box::new(StringInput::new("aaaa")));

    assert!(parser.rep_one());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::rep_one, start: 0, end: 4 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn rep_one_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("b")));

    assert!(!parser.rep_one());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn opt_empty() {
    let mut parser = Rdp::new(Box::new(StringInput::new("")));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn opt_right() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a")));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn opt_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("b")));

    assert!(parser.opt());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::opt, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pres_right() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a")));

    assert!(parser.pres());
    assert!(!parser.end());

    let queue = vec![
        Token { rule: Rule::pres, start: 0, end: 0 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn pres_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("b")));

    assert!(!parser.pres());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::pres], 0));
}

#[test]
fn abs_right() {
    let mut parser = Rdp::new(Box::new(StringInput::new("c")));

    assert!(parser.abs());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::abs, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn abs_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a")));

    assert!(!parser.abs());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::abs], 0));
}

#[test]
fn digit_right() {
    let mut parser = Rdp::new(Box::new(StringInput::new("0")));

    assert!(parser.digit());
    assert!(parser.end());

    let queue = vec![
        Token { rule: Rule::digit, start: 0, end: 1 }
    ];

    assert!(parser.queue().iter().eq(&queue));
}

#[test]
fn digit_wrong() {
    let mut parser = Rdp::new(Box::new(StringInput::new("a")));

    assert!(!parser.digit());
    assert!(!parser.end());

    let queue = vec![];

    assert!(parser.queue().iter().eq(&queue));

    assert_eq!(parser.expected(), (vec![Rule::digit], 0));
}
