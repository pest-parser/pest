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

use pest::Parser;

use pest_grammars::json::*;

// json parser             time:   [12.085 µs 12.339 µs 12.680 µs]
fn bench_json_parse(c: &mut Criterion) {
    let data = include_str!("data.json");

    c.bench_function("json parser", |b| {
        b.iter(|| JsonParser::parse(Rule::json, &data).unwrap())
    });
}

mod autocorrect {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar_inline = r#"
item = _{ SOI ~ line* ~ EOI }
line = _{ pair | other }

WHITESPACE = { " " | "\t" | NEWLINE }

other = ${ !(pair) ~ ANY }

key    = ${ inner_string ~ (" ")* ~ ":" ~ (" ")* }
string = ${ inner_string }
pair   = { key ~ string }

inner_string = @{ ("\"" ~ (!(NEWLINE | "\"") ~ ANY)* ~ "\"") }
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
fn bench_line_col(c: &mut Criterion) {
    let data = include_str!("main.i18n.json");
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

// pairs nested iter             time:   [2.0168 ms 2.0381 ms 2.0725 ms]
// pairs flatten iter            time:   [4.5973 µs 4.6132 µs 4.6307 µs]
fn bench_pairs_iter(c: &mut Criterion) {
    let data = include_str!("data.json");

    fn iter_all_pairs(pairs: pest::iterators::Pairs<autocorrect::Rule>) {
        for pair in pairs {
            iter_all_pairs(pair.into_inner());
        }
    }

    c.bench_function("pairs nested iter", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, &data).unwrap();

        b.iter(move || iter_all_pairs(pairs.clone()));
    });

    c.bench_function("pairs flatten iter", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, &data).unwrap();

        b.iter(move || {
            for _pair in pairs.clone().flatten().into_iter() {
                // do nothing
            }
        });
    });
}

criterion_group!(benches, bench_json_parse, bench_line_col, bench_pairs_iter);
criterion_main!(benches);
