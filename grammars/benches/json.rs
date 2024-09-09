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
        b.iter(|| JsonParser::parse(Rule::json, data).unwrap())
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
// pair.line_col (with LineIndex)               time:   [22.426 µs 22.489 µs 22.556 µs]
// position.line_col                            time:   [212.59 µs 213.38 µs 214.29 µs]
// position.line_col (with fast-line-col)       time:   [18.241 µs 18.382 µs 18.655 µs]
//
// With 1000 times iter
// pair.line_col (with LineIndex)               time:   [41.160 µs 41.969 µs 43.478 µs]
// position.line_col                            time:   [1.7199 ms 1.7246 ms 1.7315 ms]
// position.line_col (with fast-line-col)       time:   [48.498 µs 49.450 µs 50.877 µs]
//
// With 10K times iter
// pair.line_col                                time:   [471.75 µs 475.03 µs 478.94 µs]
// position.line_col (with fast-line-col)       time:   [8.1941 ms 8.3278 ms 8.5144 ms]
fn bench_line_col(c: &mut Criterion) {
    let data = include_str!("main.i18n.json");
    let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, data)
        .unwrap()
        .flatten();

    c.bench_function("pair.line_col", |b| {
        b.iter(|| {
            let mut pairs = pairs.clone();
            for _ in 0..1000 {
                pairs.next().unwrap().line_col();
            }
        })
    });

    c.bench_function("position.line_col", |b| {
        b.iter(|| {
            let mut pairs = pairs.clone();
            for _ in 0..1000 {
                pairs.next().unwrap().as_span().start_pos().line_col();
            }
        });
    });
}

// pairs nested iter (v2.5.2)           time:   [2.9007 µs 3.0432 µs 3.2138 µs]
// pairs nested iter (with LineIndex)   time:   [14.716 µs 14.822 µs 14.964 µs]
// pairs flatten iter (v2.5.2)          time:   [1.1230 µs 1.1309 µs 1.1428 µs]
// pairs flatten iter (with LineIndex)  time:   [5.4637 µs 5.6061 µs 5.7886 µs]
// pairs nested collect (v2.5.7)        time:   [8.4609 µs 8.4644 µs 8.4680 µs]
// pairs nested collect (ExactSize)     time:   [7.9492 µs 7.9604 µs 7.9751 µs]
// pairs flatten collect (v2.5.7)       time:   [11.471 µs 11.475 µs 11.480 µs]
// pairs flatten collect (ExactSize)    time:   [11.058 µs 11.062 µs 11.066 µs]
fn bench_pairs_iter(c: &mut Criterion) {
    let data = include_str!("data.json");

    fn iter_all_pairs(pairs: pest::iterators::Pairs<autocorrect::Rule>) {
        for pair in pairs {
            iter_all_pairs(pair.into_inner());
        }
    }

    c.bench_function("pairs nested iter", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, data).unwrap();
        b.iter(move || iter_all_pairs(pairs.clone()));
    });

    c.bench_function("pairs flatten iter", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, data).unwrap();

        b.iter(move || {
            for _pair in pairs.clone().flatten() {
                // do nothing
            }
        });
    });

    c.bench_function("pairs nested collect", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, data).unwrap();

        b.iter(move || {
            let _pairs = pairs.clone().collect::<Vec<_>>();
        });
    });

    c.bench_function("pairs flatten collect", |b| {
        let pairs = autocorrect::JsonParser::parse(autocorrect::Rule::item, data).unwrap();

        b.iter(move || {
            let _pairs = pairs.clone().flatten().collect::<Vec<_>>();
        });
    });
}

criterion_group!(benches, bench_json_parse, bench_line_col, bench_pairs_iter);
criterion_main!(benches);
