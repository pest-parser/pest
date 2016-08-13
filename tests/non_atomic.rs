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
        atomic = @{ non_atomic* }
        non_atomic = !@{ ["("] ~ [")"] }

        whitespace = _{ [" "] }
    }
}

#[test]
fn atomic() {
    let mut parser = Rdp::new(StringInput::new("()"));

    assert!(parser.atomic());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::atomic, 0, 2),
        Token::new(Rule::non_atomic, 0, 2)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn non_atomic() {
    let mut parser = Rdp::new(StringInput::new("(  )()"));

    assert!(parser.atomic());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::atomic, 0, 6),
        Token::new(Rule::non_atomic, 0, 4),
        Token::new(Rule::non_atomic, 4, 6)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn fail_atomic_with_whitespace() {
    let mut parser = Rdp::new(StringInput::new("(  ) ()"));

    assert!(parser.atomic());
    assert!(!parser.end());

    assert_eq!(parser.expected(), (vec![
        Rule::non_atomic
    ], 4));
}
