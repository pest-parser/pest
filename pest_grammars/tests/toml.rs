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

use std::fs::File;
use std::io::Read;

use pest::Parser;

use pest_grammars::toml::*;

#[test]
fn boolean() {
    parses_to! {
        parser: TomlParser,
        input: "true",
        rule: Rule::boolean,
        tokens: [
            boolean(0, 4)
        ]
    };
}

#[test]
fn integer() {
    parses_to! {
        parser: TomlParser,
        input: "+1_000_0",
        rule: Rule::integer,
        tokens: [
            integer(0, 8)
        ]
    };
}

#[test]
fn float() {
    parses_to! {
        parser: TomlParser,
        input: "+1_0.0_1e+100",
        rule: Rule::float,
        tokens: [
            float(0, 13)
        ]
    };
}

#[test]
fn partial_time() {
    parses_to! {
        parser: TomlParser,
        input: "12:34:56.000",
        rule: Rule::partial_time,
        tokens: [
            partial_time(0, 12, [
                time_hour(0, 2),
                time_minute(3, 5),
                time_second(6, 8),
                time_secfrac(8, 12)
            ])
        ]
    };
}

#[test]
fn full_date() {
    parses_to! {
        parser: TomlParser,
        input: "2001-12-13",
        rule: Rule::full_date,
        tokens: [
            full_date(0, 10, [
                date_fullyear(0, 4),
                date_month(5, 7),
                date_mday(8, 10)
            ])
        ]
    };
}

#[test]
fn local_date_time() {
    parses_to! {
        parser: TomlParser,
        input: "2001-12-13T12:34:56.000",
        rule: Rule::local_date_time,
        tokens: [
            local_date_time(0, 23, [
                full_date(0, 10, [
                    date_fullyear(0, 4),
                    date_month(5, 7),
                    date_mday(8, 10)
                ]),
                partial_time(11, 23, [
                    time_hour(11, 13),
                    time_minute(14, 16),
                    time_second(17, 19),
                    time_secfrac(19, 23)
                ])
            ])
        ]
    };
}

#[test]
fn date_time() {
    parses_to! {
        parser: TomlParser,
        input: "2001-12-13T12:34:56.000Z",
        rule: Rule::date_time,
        tokens: [
            date_time(0, 24, [
                full_date(0, 10, [
                    date_fullyear(0, 4),
                    date_month(5, 7),
                    date_mday(8, 10)
                ]),
                full_time(11, 24, [
                    partial_time(11, 23, [
                        time_hour(11, 13),
                        time_minute(14, 16),
                        time_second(17, 19),
                        time_secfrac(19, 23)
                    ]),
                    time_offset(23, 24)
                ])
            ])
        ]
    };
}

#[test]
fn literal() {
    parses_to! {
        parser: TomlParser,
        input: "'\"'",
        rule: Rule::literal,
        tokens: [
            literal(0, 3)
        ]
    };
}

#[test]
fn multi_line_literal() {
    parses_to! {
        parser: TomlParser,
        input: "'''\"'''",
        rule: Rule::multi_line_literal,
        tokens: [
            multi_line_literal(0, 7)
        ]
    };
}

#[test]
fn string() {
    parses_to! {
        parser: TomlParser,
        input: r#""\n""#,
        rule: Rule::string,
        tokens: [
            string(0, 4)
        ]
    };
}

#[test]
fn multi_line_string() {
    parses_to! {
        parser: TomlParser,
        input: r#"""" \n """"#,
        rule: Rule::multi_line_string,
        tokens: [
            multi_line_string(0, 10)
        ]
    };
}

#[test]
fn array_empty() {
    parses_to! {
        parser: TomlParser,
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
        parser: TomlParser,
        input: "['', 2017-08-09, 20.0]",
        rule: Rule::array,
        tokens: [
            array(0, 22, [
                literal(1, 3),
                full_date(5, 15, [
                    date_fullyear(5, 9),
                    date_month(10, 12),
                    date_mday(13, 15)
                ]),
                float(17, 21)
            ])
        ]
    };
}

#[test]
fn inline_table() {
    parses_to! {
        parser: TomlParser,
        input: "{ a = 'b' }",
        rule: Rule::inline_table,
        tokens: [
            inline_table(0, 11, [
                pair(2, 9, [
                    key(2, 3),
                    literal(6, 9)
                ])
            ])
        ]
    };
}

#[test]
fn table() {
    parses_to! {
        parser: TomlParser,
        input: "[a.b]\nc = 'd'",
        rule: Rule::table,
        tokens: [
            table(0, 13, [
                key(1, 2),
                key(3, 4),
                pair(6, 13, [
                    key(6, 7),
                    literal(10, 13)
                ])
            ])
        ]
    };
}

#[test]
fn array_table() {
    parses_to! {
        parser: TomlParser,
        input: "[[a.b]]\nc = 'd'",
        rule: Rule::array_table,
        tokens: [
            array_table(0, 15, [
                key(2, 3),
                key(4, 5),
                pair(8, 15, [
                    key(8, 9),
                    literal(12, 15)
                ])
            ])
        ]
    };
}

#[test]
fn examples() {
    let mut file = File::open("tests/examples.toml").unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    TomlParser::parse(Rule::toml, &data).unwrap();
}
