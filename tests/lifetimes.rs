// pest. Elegant, efficient grammars
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use pest::prelude::*;

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Sentence(&'a str),
}

impl_rdp! {
    grammar! {
        word     =  { letter* }
        letter   =  { ['a'..'z'] }
    }

    process! {
        _word(&self) -> Node<'input> {
            (&w: word) => Node::Sentence(w)
        }
    }
}

#[test]
fn word() {
    let file = "abc def";
    let result = {
        let mut parser = Rdp::new(StringInput::new(&file));

        assert!(parser.word());
        parser._word()
    };
    assert_eq!(result, Node::Sentence("abc"));
}
