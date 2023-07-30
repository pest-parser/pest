// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use criterion::{criterion_group, criterion_main, Criterion};
use pest::stack::Stack;

fn snapshot_push_restore<'i, T: Clone>(
    elements: impl Iterator<Item = T> + Clone,
    loop_times: usize,
) {
    let mut stack = Stack::<T>::new();
    for _ in 0..loop_times {
        stack.snapshot();
        for elem in elements.clone() {
            stack.push(elem);
        }
        stack.restore();
    }
}

fn snapshot_push_clear_snapshot<'i, T: Clone>(
    elements: impl Iterator<Item = T> + Clone,
    loop_times: usize,
) {
    let mut stack = Stack::<T>::new();
    for _ in 0..loop_times {
        stack.snapshot();
        for elem in elements.clone() {
            stack.push(elem);
        }
        stack.clear_snapshot();
    }
}

fn benchmark(b: &mut Criterion) {
    b.bench_function("snapshot_push_100_restore_loop_10000_small", |b| {
        b.iter(|| snapshot_push_restore(0..100, 10000))
    })
    .bench_function("snapshot_push_100_push_clear_loop_10000_small", |b| {
        b.iter(|| snapshot_push_clear_snapshot(0..100, 10000))
    });
}

criterion_group!(benchmarks, benchmark);
criterion_main!(benchmarks);
