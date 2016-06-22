// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use pest::prelude::*;

impl_rdp! {
    grammar! {
        expr = _{ paren ~ expr? }
        paren = { ["("] ~ expr? ~ [")"] }
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

    assert!(parser.expr());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::paren, 0, 4),
        Token::new(Rule::paren, 1, 3),
        Token::new(Rule::paren, 4, 12),
        Token::new(Rule::paren, 5, 9),
        Token::new(Rule::paren, 6, 8),
        Token::new(Rule::paren, 9, 11),
        Token::new(Rule::paren, 12, 14)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn fail() {
    let mut parser = Rdp::new(StringInput::new("(())((())())("));

    assert!(parser.expr());
    assert!(!parser.end());

    let queue = vec![
        Token::new(Rule::paren, 0, 4),
        Token::new(Rule::paren, 1, 3),
        Token::new(Rule::paren, 4, 12),
        Token::new(Rule::paren, 5, 9),
        Token::new(Rule::paren, 6, 8),
        Token::new(Rule::paren, 9, 11)
    ];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::paren], 13));
}

#[test]
fn rep_zero_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(parser.rep_zero());

    let queue = vec![
        Token::new(Rule::rep_zero, 0, 0)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn rep_zero_long() {
    let mut parser = Rdp::new(StringInput::new("aaaa"));

    assert!(parser.rep_zero());

    let queue = vec![
        Token::new(Rule::rep_zero, 0, 4)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn rep_zero_wrong() {
    let mut parser = Rdp::new(StringInput::new("aaaab"));

    assert!(!parser.rep_zero());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::eoi], 4));
}

#[test]
fn rep_one_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(!parser.rep_one());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn rep_one_long() {
    let mut parser = Rdp::new(StringInput::new("aaaa"));

    assert!(parser.rep_one());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::rep_one, 0, 4)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn rep_one_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(!parser.rep_one());
    assert!(!parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::rep_one], 0));
}

#[test]
fn opt_empty() {
    let mut parser = Rdp::new(StringInput::new(""));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::opt, 0, 0)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn opt_right() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(parser.opt());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::opt, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn opt_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(parser.opt());
    assert!(!parser.end());

    let queue = vec![
        Token::new(Rule::opt, 0, 0)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn pres_right() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(parser.pres());
    assert!(!parser.end());

    let queue = vec![
        Token::new(Rule::pres, 0, 0)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn pres_wrong() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(!parser.pres());
    assert!(!parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::pres], 0));
}

#[test]
fn abs_right() {
    let mut parser = Rdp::new(StringInput::new("c"));

    assert!(parser.abs());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::abs, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn abs_wrong() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(!parser.abs());
    assert!(!parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::abs], 0));
}

#[test]
fn digit_right() {
    let mut parser = Rdp::new(StringInput::new("0"));

    assert!(parser.digit());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::digit, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn digit_wrong() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(!parser.digit());
    assert!(!parser.end());

    let queue = vec![];

    assert_eq!(parser.queue(), &queue);

    assert_eq!(parser.expected(), (vec![Rule::digit], 0));
}

#[test]
fn expression() {
    let mut parser = Rdp::new(StringInput::new("1+2+3*9^2^2+2"));

    assert!(parser.expression());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::expression, 0, 13),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::plus, 1, 2),
        Token::new(Rule::number, 2, 3),
        Token::new(Rule::plus, 3, 4),
        Token::new(Rule::mul, 4, 11),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::times, 5, 6),
        Token::new(Rule::pow, 6, 11),
        Token::new(Rule::number, 6, 7),
        Token::new(Rule::power, 7, 8),
        Token::new(Rule::pow, 8, 11),
        Token::new(Rule::number, 8, 9),
        Token::new(Rule::power, 9, 10),
        Token::new(Rule::number, 10, 11),
        Token::new(Rule::plus, 11, 12),
        Token::new(Rule::number, 12, 13)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn expression_spaced() {
    let mut parser = Rdp::new(StringInput::new("1 + 2 + 3 * 9^2^2 + 2"));

    assert!(parser.expression());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::expression, 0, 21),
        Token::new(Rule::number, 0, 1),
        Token::new(Rule::plus, 2, 3),
        Token::new(Rule::number, 4, 5),
        Token::new(Rule::plus, 6, 7),
        Token::new(Rule::mul, 8, 17),
        Token::new(Rule::number, 8, 9),
        Token::new(Rule::times, 10, 11),
        Token::new(Rule::pow, 12, 17),
        Token::new(Rule::number, 12, 13),
        Token::new(Rule::power, 13, 14),
        Token::new(Rule::pow, 14, 17),
        Token::new(Rule::number, 14, 15),
        Token::new(Rule::power, 15, 16),
        Token::new(Rule::number, 16, 17),
        Token::new(Rule::plus, 18, 19),
        Token::new(Rule::number, 20, 21)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn expression_space_after() {
    let mut parser = Rdp::new(StringInput::new("1 "));

    assert!(parser.expression());
    assert!(!parser.end());

    let queue = vec![
        Token::new(Rule::expression, 0, 1),
        Token::new(Rule::number, 0, 1)
    ];

    assert_eq!(parser.queue(), &queue);
}
