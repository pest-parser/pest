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
        abc = { ["a"] ~ ["b"] | ["a c"] }

        whitespace = { [" "] }
    }
}

#[test]
fn whitespace_token() {
    let mut parser = Rdp::new(StringInput::new("a b"));

    assert!(parser.abc());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::abc, 0, 3),
        Token::new(Rule::whitespace, 1, 2)
    ];

    assert_eq!(parser.queue(), &queue);
}

#[test]
fn no_whitespace_token() {
    let mut parser = Rdp::new(StringInput::new("a c"));

    assert!(parser.abc());
    assert!(parser.end());

    let queue = vec![
        Token::new(Rule::abc, 0, 3)
    ];

    assert_eq!(parser.queue(), &queue);
}
