// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;
extern crate pest;
extern crate pest_grammars;

use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use test::Bencher;

use pest::span::Span;
use pest::iterators::Pair;
use pest::Parser;

use pest_grammars::json::*;

enum Json<'i> {
    Null,
    Bool(bool),
    Number(f64),
    String(Span<'i>),
    Array(Vec<Json<'i>>),
    Object(HashMap<Span<'i>, Json<'i>>)
}

fn consume<'i>(pair: Pair<'i, Rule>) -> Json<'i> {
    fn value<'i>(pair: Pair<'i, Rule>) -> Json<'i> {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::null => Json::Null,
            Rule::bool => {
                match pair.as_str() {
                    "false" => Json::Bool(false),
                    "true" => Json::Bool(true),
                    _ => unreachable!()
                }
            }
            Rule::number => {
                Json::Number(pair.as_str().parse().unwrap())
            }
            Rule::string => {
                Json::String(pair.into_span())
            }
            Rule::array => {
                Json::Array(pair.into_inner().map(value).collect())
            }
            Rule::object => {
                let pairs = pair.into_inner().map(|pos| {
                    let mut pair = pos.into_inner();

                    let key = pair.next().unwrap().into_span();
                    let value = value(pair.next().unwrap());

                    (key, value)
                });

                Json::Object(pairs.collect())
            }
            _ => unreachable!()
        }
    }

    value(pair)
}

#[bench]
fn data(b: &mut Bencher) {
    let mut file = File::open("benches/data.json").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    b.iter(|| {
        consume(JsonParser::parse(Rule::json, &data).unwrap().next().unwrap())
    });
}
