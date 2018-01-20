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

use std::collections::HashMap;

use pest::{state, Error, Parser, ParserState};
use pest::Position;
use pest::Span;
use pest::iterators::{Pair, Pairs};

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    json,
    object,
    pair,
    array,
    value,
    string,
    escape,
    unicode,
    hex,
    number,
    int,
    exp,
    bool,
    null
}

struct JsonParser;

impl Parser<Rule> for JsonParser {
    fn parse(rule: Rule, input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        fn json<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            value(pos, state)
        }

        fn object<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::object, pos, |state, pos| {
                state
                    .sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("{")
                                .and_then(|pos| skip(pos, state))
                                .and_then(|pos| pair(pos, state))
                                .and_then(|pos| skip(pos, state))
                                .and_then(|pos| {
                                    pos.repeat(|pos| {
                                        state.sequence(move |state| {
                                            pos.sequence(|pos| {
                                                pos.match_string(",")
                                                    .and_then(|pos| skip(pos, state))
                                                    .and_then(|pos| pair(pos, state))
                                                    .and_then(|pos| skip(pos, state))
                                            })
                                        })
                                    })
                                })
                                .and_then(|pos| pos.match_string("}"))
                        })
                    })
                    .or_else(|pos| {
                        state.sequence(move |state| {
                            pos.sequence(|pos| {
                                pos.match_string("{")
                                    .and_then(|pos| skip(pos, state))
                                    .and_then(|pos| pos.match_string("}"))
                            })
                        })
                    })
            })
        }

        fn pair<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::pair, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        string(pos, state)
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| pos.match_string(":"))
                            .and_then(|pos| skip(pos, state))
                            .and_then(|pos| value(pos, state))
                    })
                })
            })
        }

        fn array<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::array, pos, |state, pos| {
                state
                    .sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("[")
                                .and_then(|pos| skip(pos, state))
                                .and_then(|pos| value(pos, state))
                                .and_then(|pos| skip(pos, state))
                                .and_then(|pos| {
                                    pos.repeat(|pos| {
                                        state.sequence(move |state| {
                                            pos.sequence(|pos| {
                                                pos.match_string(",")
                                                    .and_then(|pos| skip(pos, state))
                                                    .and_then(|pos| value(pos, state))
                                                    .and_then(|pos| skip(pos, state))
                                            })
                                        })
                                    })
                                })
                                .and_then(|pos| pos.match_string("]"))
                        })
                    })
                    .or_else(|pos| {
                        state.sequence(move |state| {
                            pos.sequence(|pos| {
                                pos.match_string("[")
                                    .and_then(|pos| skip(pos, state))
                                    .and_then(|pos| pos.match_string("]"))
                            })
                        })
                    })
            })
        }

        fn value<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::value, pos, |state, pos| {
                string(pos, state)
                    .or_else(|pos| number(pos, state))
                    .or_else(|pos| object(pos, state))
                    .or_else(|pos| array(pos, state))
                    .or_else(|pos| bool(pos, state))
                    .or_else(|pos| null(pos, state))
            })
        }

        fn string<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::string, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("\"")
                        .and_then(|pos| {
                            pos.repeat(|pos| {
                                escape(pos, state).or_else(|pos| {
                                    pos.sequence(|pos| {
                                        state
                                            .lookahead(false, move |_| {
                                                pos.lookahead(false, |pos| {
                                                    pos.match_string("\"")
                                                        .or_else(|pos| pos.match_string("\\"))
                                                })
                                            })
                                            .and_then(|pos| pos.skip(1))
                                    })
                                })
                            })
                        })
                        .and_then(|pos| pos.match_string("\""))
                })
            })
        }

        fn escape<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("\\").and_then(|pos| {
                    pos.match_string("\"")
                        .or_else(|pos| pos.match_string("\\"))
                        .or_else(|pos| pos.match_string("/"))
                        .or_else(|pos| pos.match_string("b"))
                        .or_else(|pos| pos.match_string("f"))
                        .or_else(|pos| pos.match_string("n"))
                        .or_else(|pos| pos.match_string("r"))
                        .or_else(|pos| pos.match_string("t"))
                        .or_else(|pos| unicode(pos, state))
                })
            })
        }

        fn unicode<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("u")
                    .and_then(|pos| hex(pos, state))
                    .and_then(|pos| hex(pos, state))
                    .and_then(|pos| hex(pos, state))
            })
        }

        fn hex<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.match_range('0'..'9')
                .or_else(|pos| pos.match_range('a'..'f'))
                .or_else(|pos| pos.match_range('A'..'F'))
        }

        fn number<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::number, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.optional(|pos| pos.match_string("-"))
                        .and_then(|pos| int(pos, state))
                        .and_then(|pos| {
                            pos.optional(|pos| {
                                pos.sequence(|pos| {
                                    pos.match_string(".")
                                        .and_then(|pos| pos.match_range('0'..'9'))
                                        .and_then(|pos| pos.repeat(|pos| pos.match_range('0'..'9')))
                                        .and_then(|pos| pos.optional(|pos| exp(pos, state)))
                                        .or_else(|pos| exp(pos, state))
                                })
                            })
                        })
                })
            })
        }

        fn int<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.match_string("0").or_else(|pos| {
                pos.sequence(|pos| {
                    pos.match_range('1'..'9')
                        .and_then(|pos| pos.repeat(|pos| pos.match_range('0'..'9')))
                })
            })
        }

        fn exp<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.sequence(|pos| {
                pos.match_string("E")
                    .or_else(|pos| pos.match_string("e"))
                    .and_then(|pos| {
                        pos.optional(|pos| {
                            pos.match_string("+").or_else(|pos| pos.match_string("-"))
                        })
                    })
                    .and_then(|pos| int(pos, state))
            })
        }

        fn bool<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::bool, pos, |_, pos| {
                pos.match_string("true")
                    .or_else(|pos| pos.match_string("false"))
            })
        }

        fn null<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::null, pos, |_, pos| pos.match_string("null"))
        }

        fn skip<'i>(
            pos: Position<'i>,
            _: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            pos.repeat(|pos| {
                pos.match_string(" ")
                    .or_else(|pos| pos.match_string("\t"))
                    .or_else(|pos| pos.match_string("\r"))
                    .or_else(|pos| pos.match_string("\n"))
            })
        }

        state(input, move |mut state, pos| match rule {
            Rule::json => json(pos, &mut state),
            Rule::object => object(pos, &mut state),
            Rule::pair => pair(pos, &mut state),
            Rule::array => array(pos, &mut state),
            Rule::value => value(pos, &mut state),
            Rule::string => string(pos, &mut state),
            Rule::escape => escape(pos, &mut state),
            Rule::unicode => unicode(pos, &mut state),
            Rule::hex => hex(pos, &mut state),
            Rule::number => number(pos, &mut state),
            Rule::int => int(pos, &mut state),
            Rule::exp => exp(pos, &mut state),
            Rule::bool => bool(pos, &mut state),
            Rule::null => null(pos, &mut state)
        })
    }
}

#[derive(Debug, PartialEq)]
enum Json<'i> {
    Null,
    Bool(bool),
    Number(f64),
    String(Span<'i>),
    Array(Vec<Json<'i>>),
    Object(HashMap<Span<'i>, Json<'i>>)
}

fn consume(pair: Pair<Rule>) -> Json {
    fn value(pair: Pair<Rule>) -> Json {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::null => Json::Null,
            Rule::bool => match pair.as_str() {
                "false" => Json::Bool(false),
                "true" => Json::Bool(true),
                _ => unreachable!()
            },
            Rule::number => Json::Number(pair.as_str().parse().unwrap()),
            Rule::string => Json::String(pair.into_span()),
            Rule::array => Json::Array(pair.into_inner().map(value).collect()),
            Rule::object => {
                let pairs = pair.into_inner().map(|pos| {
                    let mut pair = pos.into_inner();

                    let key = pair.next().unwrap().into_span();
                    let value = value(pair.next().unwrap());

                    (key, value)
                });

                Json::Object(pairs.collect())
            }
            _ => unreachable!()
        }
    }

    value(pair)
}

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
fn ast() {
    let input = "{\"a\": [null, true, 3.4]}";
    let start = Position::from_start(input).skip(1).unwrap();
    let end = start.clone().skip(3).unwrap();
    let span = start.span(&end);

    let mut pairs = HashMap::new();
    pairs.insert(
        span,
        Json::Array(vec![Json::Null, Json::Bool(true), Json::Number(3.4)])
    );

    let ast = consume(
        JsonParser::parse(Rule::json, input)
            .unwrap()
            .next()
            .unwrap()
    );

    assert_eq!(ast, Json::Object(pairs));
}
