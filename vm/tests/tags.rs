// pest. The Elegant Parser
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![cfg(feature = "grammar-extras")]

extern crate pest;
extern crate pest_meta;
extern crate pest_vm;

use pest_meta::parser::Rule;
use pest_meta::{optimizer, parser};
use pest_vm::Vm;

const GRAMMAR: &str = include_str!("../../derive/tests/tags.pest");

fn vm() -> Vm {
    let pairs = parser::parse(Rule::grammar_rules, GRAMMAR).unwrap();
    let ast = parser::consume_rules(pairs).unwrap();
    Vm::new(optimizer::optimize(ast))
}

/// Start position and tag of every `a` pair, in order.
fn tags(rule: &str, input: &str) -> Vec<(usize, Option<String>)> {
    let vm = vm();
    vm.parse(rule, input)
        .unwrap()
        .flatten()
        .filter(|pair| pair.as_rule() == "a")
        .map(|pair| {
            (
                pair.as_span().start(),
                pair.as_node_tag().map(str::to_owned),
            )
        })
        .collect()
}

fn t(pos: usize) -> (usize, Option<String>) {
    (pos, Some("t".to_owned()))
}

#[test]
fn repeat_tags_every_pair() {
    assert_eq!(tags("rep", "a a a."), [t(0), t(2), t(4)]);
}

#[test]
fn repeat_tags_every_pair_atomic() {
    assert_eq!(tags("rep_atomic", "aaa."), [t(0), t(1), t(2)]);
}

#[test]
fn repeat_once_tags_every_pair() {
    assert_eq!(tags("rep_once", "a a a."), [t(0), t(2), t(4)]);
}

#[test]
fn empty_optional_tags_nothing() {
    assert_eq!(tags("optional", "a."), [(0, None)]);
    assert_eq!(tags("optional", "a a."), [(0, None), t(2)]);
}

#[test]
fn literal_tags_nothing() {
    assert_eq!(tags("literal", "a q"), [(0, None)]);
}

#[test]
fn lookahead_tags_nothing() {
    assert_eq!(tags("lookahead", "a a"), [(0, None), (2, None)]);
}

#[test]
fn choice_tags_only_its_branch() {
    assert_eq!(tags("choice", "a q"), [(0, None)]);
    assert_eq!(tags("choice", "a a"), [(0, None), t(2)]);
}

#[test]
fn sequence_tags_its_pairs() {
    assert_eq!(tags("sequence", "a a a"), [t(0), t(2), (4, None)]);
}

#[test]
fn silent_rule_tag_does_not_leak() {
    assert_eq!(tags("silent_leak", "a q"), [(0, None)]);
}

#[test]
fn empty_optional_does_not_retag() {
    assert_eq!(tags("prefix", "a"), [(0, Some("p".to_owned()))]);
}

#[test]
fn optional_tag_matches_issue_984() {
    // #984: `#prefix=(STAR)? ~ #suffix=DOT?` on "*" tagged STAR as "suffix".
    let grammar =
        "expr = { SOI ~ #prefix=(STAR)? ~ #suffix=DOT? ~ EOI }\nSTAR={\"*\"}\nDOT={\".\"}";
    let pairs = parser::parse(Rule::grammar_rules, grammar).unwrap();
    let vm = Vm::new(optimizer::optimize(parser::consume_rules(pairs).unwrap()));
    let pairs = vm.parse("expr", "*").unwrap();
    assert!(pairs.find_first_tagged("prefix").is_some());
    assert!(pairs.find_first_tagged("suffix").is_none());
}
