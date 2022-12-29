// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate criterion;
extern crate pest;
extern crate pest_grammars;
use criterion::{criterion_group, criterion_main, Criterion};

use std::fs::File;
use std::io::Read;

use pest::Parser;

use pest_grammars::json::*;

fn criterion_benchmark(c: &mut Criterion) {
    let mut file = File::open("benches/data.json").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    c.bench_function("json parser", |b| {
        b.iter(|| JsonParser::parse(Rule::json, &data).unwrap())
    });
}

mod autocorrect {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar_inline = r#"
newline = ${ "\n" | "\r" }
space = ${ " "+ }

other = ${ !(pair) ~ ANY }
comment = ${ single_line_comment | multiline_comment }
single_line_comment = _{ "//" ~ (!(newline) ~ ANY)* }
multiline_comment = _{ "/*" ~ (!("*/") ~ ANY)* ~ "*/"}

string_type = _{ 
  ("\"" ~ (!(newline | "\"") ~ ANY)* ~ "\"") 
}
key = ${ string_type ~ (" ")* ~ ":" ~ (" ")* }
string = ${ string_type  }
pair = _{ key ~ string }

line = _{ pair | comment | space | other | newline }
item = _{ SOI ~ line* ~ EOI } 
"#]
    pub struct JsonParser;
}

// With 500 times iter
// pair.line_col                                time:   [2.9937 µs 2.9975 µs 3.0018 µs]
// position.line_col                            time:   [212.59 µs 213.38 µs 214.29 µs]
// position.line_col (with fast-line-col)       time:   [18.241 µs 18.382 µs 18.655 µs]
//
// With 1000 times iter
// pair.line_col                                time:   [10.814 µs 10.846 µs 10.893 µs]
// position.line_col                            time:   [90.135 µs 93.901 µs 98.655 µs]
// position.line_col (with fast-line-col)       time:   [1.7199 ms 1.7246 ms 1.7315 ms]
fn line_col_benchmark(c: &mut Criterion) {
    let mut file = File::open("benches/main.i18n.json").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();
    let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, &data).unwrap();

    c.bench_function("pair.line_col", |b| {
        b.iter(|| {
            let mut pairs = pairs.clone();
            for _ in 0..500 {
                pairs.next().unwrap().line_col();
            }
        })
    });

    c.bench_function("position.line_col", |b| {
        b.iter(|| {
            let mut pairs = pairs.clone();
            for _ in 0..500 {
                pairs.next().unwrap().as_span().start_pos().line_col();
            }
        });
    });
}

criterion_group!(benches, criterion_benchmark, line_col_benchmark,);
criterion_main!(benches);
