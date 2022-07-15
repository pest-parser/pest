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

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
