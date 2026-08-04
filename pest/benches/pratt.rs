// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pest::pratt_parser::{Affix, Assoc, ConstPrattParser, Op, PrattParser, PrattParserOps};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
    B8,
    B9,
    C0,
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8,
    C9,
}

const RULES: &[Rule] = &[
    Rule::A0,
    Rule::A1,
    Rule::A2,
    Rule::A3,
    Rule::A4,
    Rule::A5,
    Rule::A6,
    Rule::A7,
    Rule::A8,
    Rule::A9,
    Rule::B0,
    Rule::B1,
    Rule::B2,
    Rule::B3,
    Rule::B4,
    Rule::B5,
    Rule::B6,
    Rule::B7,
    Rule::B8,
    Rule::B9,
    Rule::C0,
    Rule::C1,
    Rule::C2,
    Rule::C3,
    Rule::C4,
    Rule::C5,
    Rule::C6,
    Rule::C7,
    Rule::C8,
    Rule::C9,
];

fn build_runtime() -> PrattParser<Rule> {
    let mut parser = PrattParser::new();
    for &rule in RULES {
        parser = parser.op(Op::infix(rule, Assoc::Left));
    }
    parser
}

fn build_const() -> ConstPrattParser<Rule> {
    let mut ops = Vec::new();
    for (i, &rule) in RULES.iter().enumerate() {
        ops.push((rule, Affix::Infix(Assoc::Left), (i as u32) + 1));
    }
    let ops: &'static [(Rule, Affix, u32)] = Box::leak(ops.into_boxed_slice());
    ConstPrattParser::new_const(ops)
}

fn benchmark(b: &mut Criterion) {
    let runtime = build_runtime();
    let const_ = build_const();

    b.bench_function("pratt_runtime_lookup", |b| {
        b.iter(|| {
            for &rule in RULES {
                black_box(runtime.get(&rule));
            }
        })
    });

    b.bench_function("pratt_const_lookup", |b| {
        b.iter(|| {
            for &rule in RULES {
                black_box(const_.get(&rule));
            }
        })
    });
}

criterion_group!(benchmarks, benchmark);
criterion_main!(benchmarks);
