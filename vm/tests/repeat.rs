// pest. The Elegant Parser
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate pest;
extern crate pest_meta;
extern crate pest_vm;

use pest_vm::Vm;

fn vm(grammar: &str) -> Vm {
    let (_, rules) = pest_meta::parse_and_optimize(grammar).unwrap();
    Vm::new(rules)
}

fn rules_matched(grammar: &str, rule: &str, input: &str) -> Vec<String> {
    let vm = vm(grammar);
    vm.parse(rule, input)
        .unwrap()
        .flatten()
        .map(|pair| pair.as_rule().to_owned())
        .collect()
}

// `PEEK_ALL*` on an empty stack matches nothing and changes nothing: it must end.
#[test]
fn repeat_of_peek_all_on_empty_stack_ends() {
    let pairs = rules_matched("r = { PEEK_ALL* ~ \"x\" }", "r", "x");
    assert_eq!(pairs, ["r"]);
}

// A repetition whose iterations consume nothing stops after the same number of iterations
// however the repeated expression is written: an alternative that is tried and backtracked
// inside an iteration does not count as progress.
#[test]
fn repeat_without_progress_stops_independently_of_shape() {
    // `s` matches nothing on empty input. Written with one rule `t` in every branch, the
    // optimizer factors the choices; with four rules of the same body (`t`, `u`, `v`, `w`)
    // it cannot, so failed branches are tried and backtracked inside each iteration. The
    // repetition must stop after the same number of iterations either way.
    let factored = "r = { s* }\n\
        s = ${ ((t ~ ANY) | (t ~ PEEK_ALL)) ~ (SOI ~ EOI) | ((t ~ ANY) | (t ~ PEEK_ALL)) }\n\
        t = !{ SOI ~ PEEK_ALL }";
    let unfactored = "r = { s* }\n\
        s = ${ ((t ~ ANY) | (u ~ PEEK_ALL)) ~ (SOI ~ EOI) | ((v ~ ANY) | (w ~ PEEK_ALL)) }\n\
        t = !{ SOI ~ PEEK_ALL }\nu = !{ SOI ~ PEEK_ALL }\n\
        v = !{ SOI ~ PEEK_ALL }\nw = !{ SOI ~ PEEK_ALL }";
    // compare iteration counts: how many `s` pairs each produced
    let count = |g: &str| {
        rules_matched(g, "r", "")
            .iter()
            .filter(|n| *n == "s")
            .count()
    };
    assert_eq!(count(unfactored), count(factored));
}
