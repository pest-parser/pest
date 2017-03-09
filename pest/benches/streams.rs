// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;

extern crate futures;

extern crate pest;

use std::sync::Arc;
use std::thread;

use test::Bencher;

use futures::future::Future;
use futures::stream::Stream;

use pest::{Error, Input, Parser, state, StringInput};
use pest::streams::{ParserStream, TokenStream};
use pest::tokens::Token;

const TOKENS: usize = 1024;

struct SynchronousParser;
struct ArcParser;

impl Parser<()> for SynchronousParser {
    fn parse<I: Input>(_: (), input: &I) -> ParserStream<()> {
        let (stream, mut state) = state(input);

        for _ in 0..TOKENS / 2 {
            state.send(Token::Start { rule: (), pos: 0 });
            state.send(Token::End   { rule: (), pos: 5 });
        }

        stream
    }
}

#[bench]
fn synchronous(b: &mut Bencher) {
    b.iter(|| {
        let input = StringInput::new("hello");
        let stream = SynchronousParser::parse((), &input);

//        stream.sliced().map(|pair| {
//            pair.expand((), |data, _| {
//                data.wait().unwrap().capture(&input).to_owned()
//            }).0
//        }).collect().wait()
    });
}

#[bench]
fn vec(b: &mut Bencher) {
    b.iter(|| {
        let mut vec: Vec<Result<Token<()>, Error<()>>> = vec![];

        for _ in 0..TOKENS / 2 {
            vec.push(Ok(Token::Start { rule: (), pos: 0 }));
            vec.push(Ok(Token::End   { rule: (), pos: 5 }));
        }

        vec
    });
}
