use criterion::{criterion_group, criterion_main, Criterion};

use std::fs::File;
use std::io::Read;

use pest::Parser;

use pest_grammars::http::*;

fn criterion_benchmark(c: &mut Criterion) {
    let mut file = File::open("benches/requests.http").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    c.bench_function("http parser", |b| {
        b.iter(|| HttpParser::parse(Rule::http, &data).unwrap())
    });
}

criterion_group!(benches, criterion_benchmark,);
criterion_main!(benches);
