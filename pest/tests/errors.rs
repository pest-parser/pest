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
        ab = { a | b }
        a  = { ["a"] }
        b  = { ["b"] }
    }

    process! {
        primary(&self) -> Result<String, String> {
            (_: ab, res: secondary()) => Ok(try!(res) + "b")
        }

        secondary(&self) -> Result<String, String> {
            (&a: a) => Ok(a.to_owned()),
            (_: b)  => Err("error".to_owned())
        }
    }
}

#[test]
fn a() {
    let mut parser = Rdp::new(StringInput::new("a"));

    assert!(parser.ab());

    assert_eq!(parser.primary(), Ok("ab".to_owned()));
}

#[test]
fn b() {
    let mut parser = Rdp::new(StringInput::new("b"));

    assert!(parser.ab());

    assert_eq!(parser.primary(), Err("error".to_owned()));
}
