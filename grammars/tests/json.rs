// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[macro_use]
extern crate pest;
extern crate pest_grammars;

use pest::Parser;
use pest_grammars::json::*;
use pretty_assertions::assert_eq;

#[test]
fn null() {
    parses_to! {
        parser: JsonParser,
        input: "null",
        rule: Rule::null,
        tokens: [
            null(0, 4)
        ]
    };
}

#[test]
fn bool() {
    parses_to! {
        parser: JsonParser,
        input: "false",
        rule: Rule::bool,
        tokens: [
            bool(0, 5)
        ]
    };
}

#[test]
fn number_zero() {
    parses_to! {
        parser: JsonParser,
        input: "0",
        rule: Rule::number,
        tokens: [
            number(0, 1)
        ]
    };
}

#[test]
fn float() {
    parses_to! {
        parser: JsonParser,
        input: "100.001",
        rule: Rule::number,
        tokens: [
            number(0, 7)
        ]
    };
}

#[test]
fn float_with_exp() {
    parses_to! {
        parser: JsonParser,
        input: "100.001E+100",
        rule: Rule::number,
        tokens: [
            number(0, 12)
        ]
    };
}

#[test]
fn number_minus_zero() {
    parses_to! {
        parser: JsonParser,
        input: "-0",
        rule: Rule::number,
        tokens: [
            number(0, 2)
        ]
    };
}

#[test]
fn string_with_escapes() {
    parses_to! {
        parser: JsonParser,
        input: "\"asd\\u0000\\\"\"",
        rule: Rule::string,
        tokens: [
            string(0, 13)
        ]
    };
}

#[test]
fn array_empty() {
    parses_to! {
        parser: JsonParser,
        input: "[ ]",
        rule: Rule::array,
        tokens: [
            array(0, 3)
        ]
    };
}

#[test]
fn array() {
    parses_to! {
        parser: JsonParser,
        input: "[0.0e1, false, null, \"a\", [0]]",
        rule: Rule::array,
        tokens: [
            array(0, 30, [
                value(1,  6, [number(1, 6)]),
                value(8, 13, [bool(8, 13)]),
                value(15, 19, [null(15, 19)]),
                value(21, 24, [string(21, 24)]),
                value(26, 29, [
                    array(26, 29, [
                        value(27, 28, [number(27, 28)])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn object() {
    parses_to! {
        parser: JsonParser,
        input: "{\"a\" : 3, \"b\" : [{}, 3]}",
        rule: Rule::object,
        tokens: [
            object(0, 24, [
                pair(1, 8, [
                    string(1, 4),
                    value(7, 8, [number(7, 8)])
                ]),
                pair(10, 23, [
                    string(10, 13),
                    value(16, 23, [
                        array(16, 23, [
                            value(17, 19, [object(17, 19)]),
                            value(21, 22, [number(21, 22)])
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_json_parse() {
    let raw = include_str!("examples.json");
    let ast = JsonParser::parse(Rule::json, raw);
    assert!(ast.is_ok());
}

#[test]
fn test_line_col() {
    let raw = include_str!("examples.json");
    let pairs = JsonParser::parse(Rule::json, raw).unwrap();
    let expected = include_str!("examples.line-col.txt");

    // Test for flatten iter, and use span.start_pos().line_col()
    let mut out = String::new();
    for pair in pairs.clone().flatten() {
        let sub_pairs = pair.clone().into_inner();
        if sub_pairs.count() == 0 {
            let span = pair.as_span();
            out.push_str(&build_line_col(span.start_pos().line_col(), span.as_str()));
        }
    }
    assert_eq!(expected.trim(), out.trim());

    // Test for nested iter, use pair.line_col()
    let mut out = String::new();
    for pair in pairs.clone() {
        out.push_str(&build_result_for_pair(pair.clone()));
    }
    assert_eq!(expected.trim(), out.trim());

    // Test for nested iter, use pair.line_col()
    let mut out = String::new();
    for pair in pairs.clone().rev() {
        out.insert_str(0, &build_result_for_pair(pair.clone()));
    }
    assert_eq!(expected.trim(), out.trim());

    // Test for flatten iter, and use pair.line_col()
    let mut out = String::new();
    for pair in pairs.clone().flatten() {
        let sub_pairs = pair.clone().into_inner();
        if sub_pairs.count() == 0 {
            let span = pair.as_span();
            out.push_str(&build_line_col(pair.line_col(), span.as_str()));
        }
    }
    assert_eq!(expected.trim(), out.trim());

    // Test for flatten rev iter, and use pair.line_col()
    let mut out = String::new();
    for pair in pairs.clone().flatten().rev() {
        let sub_pairs = pair.clone().into_inner();
        if sub_pairs.count() == 0 {
            let span = pair.as_span();
            out.insert_str(0, &build_line_col(pair.line_col(), span.as_str()));
        }
    }
    assert_eq!(expected.trim(), out.trim());
}

fn build_line_col(line_col: (usize, usize), str: &str) -> String {
    format!(
        "({}:{}) {}\n",
        line_col.0,
        line_col.1,
        str.replace('\n', "\\n")
    )
}

fn build_result_for_pair(pair: pest::iterators::Pair<Rule>) -> String {
    let mut out = String::new();

    let sub_pairs = pair.clone().into_inner();

    if sub_pairs.clone().count() == 0 {
        out.push_str(&build_line_col(pair.line_col(), pair.as_str()));
    } else {
        for sub_pair in sub_pairs {
            out.push_str(&build_result_for_pair(sub_pair));
        }
    }
    out
}
