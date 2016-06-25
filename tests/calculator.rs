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
        expression = _{
            { ["("] ~ expression ~ [")"] | number }
            addition       = { plus  | minus }
            multiplication = { times | slash }
        }
        number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
        plus   =  { ["+"] }
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }

        whitespace = _{ [" "] }
    }

    process! {
        compute(&self) -> i32 {
            (&number: number) => number.parse::<i32>().unwrap(),
            (_: addition, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::plus  => left + right,
                    Rule::minus => left - right,
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::times => left * right,
                    Rule::slash => left / right,
                    _ => unreachable!()
                }
            }
        }
    }
}

#[test]
fn zero() {
    let mut parser = Rdp::new(StringInput::new("0"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 0);
}

#[test]
fn number() {
    let mut parser = Rdp::new(StringInput::new("123"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 123);
}

#[test]
fn addition() {
    let mut parser = Rdp::new(StringInput::new("123+321"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 444);
}

#[test]
fn subtraction() {
    let mut parser = Rdp::new(StringInput::new("123-321"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), -198);
}

#[test]
fn multiplication() {
    let mut parser = Rdp::new(StringInput::new("16*16"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 256);
}

#[test]
fn division() {
    let mut parser = Rdp::new(StringInput::new("16/16"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 1);
}

#[test]
fn precedence() {
    let mut parser = Rdp::new(StringInput::new("2+3*4"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 14);
}

#[test]
fn parens() {
    let mut parser = Rdp::new(StringInput::new("(2+3)*4"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 20);
}

#[test]
fn negative() {
    let mut parser = Rdp::new(StringInput::new("-2*-2 / -4"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), -1);
}

#[test]
fn complex() {
    let mut parser = Rdp::new(StringInput::new("(3 + (9 + 3 * 4 + (3 + 1) / 2 - 4)) * 2"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 44);
}
