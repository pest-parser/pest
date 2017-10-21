// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "../tests/reporting.pest"]
struct ReportingParser;

#[test]
fn choices() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::choices,
        positives: vec![Rule::a, Rule::b, Rule::c],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn choices_no_progress() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::choices_no_progress,
        positives: vec![Rule::choices_no_progress],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn choices_a_progress() {
    fails_with! {
        parser: ReportingParser,
        input: "a",
        rule: Rule::choices_a_progress,
        positives: vec![Rule::a],
        negatives: vec![],
        pos: 1
    };
}

#[test]
fn choices_b_progress() {
    fails_with! {
        parser: ReportingParser,
        input: "b",
        rule: Rule::choices_b_progress,
        positives: vec![Rule::b],
        negatives: vec![],
        pos: 1
    };
}

#[test]
fn nested() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::level1,
        positives: vec![Rule::a, Rule::b, Rule::c],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn negative() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::negative,
        positives: vec![],
        negatives: vec![Rule::d],
        pos: 0
    };
}

#[test]
fn negative_match() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::negative_match,
        positives: vec![Rule::b],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn mixed() {
    fails_with! {
        parser: ReportingParser,
        input: "x",
        rule: Rule::mixed,
        positives: vec![Rule::a],
        negatives: vec![Rule::d],
        pos: 0
    };
}
