// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate futures;
#[macro_use]
extern crate pest;

use std::collections::HashMap;
use std::sync::Arc;

use pest::inputs::{Input, Position};
use pest::{Parser, ParserState, state};
use pest::streams::{ParserStream, TokenStream};

#[allow(non_camel_case_types)]
#[allow(dead_code)]
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
    fn parse<I: Input + 'static>(rule: Rule, input: Arc<I>) -> ParserStream<Rule, I> {
        fn json<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::json, pos, must_match, |pos, state| {
                value(pos, state, must_match)
            })
        }

        fn object<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
                          -> Result<Position<I>, Position<I>> {

            state.rule(Rule::object, pos, must_match, |pos, state| {
                pos.sequence(|p| {
                    p.match_string("{").and_then(|p| {
                        skip(p, state, false)
                    }).and_then(|p| {
                        pair(p, state, false)
                    }).and_then(|p| {
                        skip(p, state, false)
                    }).and_then(|p| {
                        p.repeat(|p| {
                            p.sequence(|p| {
                                p.match_string(",").and_then(|p| {
                                    skip(p, state, false)
                                }).and_then(|p| {
                                    pair(p, state, false)
                                }).and_then(|p| {
                                    skip(p, state, false)
                                })
                            })
                        })
                    }).and_then(|p| {
                        p.match_string("}")
                    })
                }).or_else(|p| {
                    p.sequence(|p| {
                        p.match_string("{").and_then(|p| {
                            skip(p, state, must_match)
                        }).and_then(|p| {
                            p.match_string("}")
                        })
                    })
                })
            })
        }

        fn pair<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
                          -> Result<Position<I>, Position<I>> {

            state.rule(Rule::pair, pos, must_match, |pos, state| {
                string(pos, state, must_match).and_then(|p| {
                    skip(p, state, must_match)
                }).and_then(|p| {
                    p.match_string(":")
                }).and_then(|p| {
                    skip(p, state, must_match)
                }).and_then(|p| {
                    value(p, state, must_match)
                })
            })
        }

        fn array<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::array, pos, must_match, |pos, state| {
                pos.sequence(|p| {
                    p.match_string("[").and_then(|p| {
                        skip(p, state, false)
                    }).and_then(|p| {
                        value(p, state, false)
                    }).and_then(|p| {
                        skip(p, state, false)
                    }).and_then(|p| {
                        p.repeat(|p| {
                            p.sequence(|p| {
                                p.match_string(",").and_then(|p| {
                                    skip(p, state, false)
                                }).and_then(|p| {
                                    value(p, state, false)
                                }).and_then(|p| {
                                    skip(p, state, false)
                                })
                            })
                        })
                    }).and_then(|p| {
                        p.match_string("]")
                    })
                }).or_else(|p| {
                    p.sequence(|p| {
                        p.match_string("[").and_then(|p| {
                            skip(p, state, must_match)
                        }).and_then(|p| {
                            p.match_string("]")
                        })
                    })
                })
            })
        }

        fn value<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::value, pos, must_match, |pos, state| {
                string(pos, state, false).or_else(|p| {
                    number(p, state, false)
                }).or_else(|p| {
                    object(p, state, false)
                }).or_else(|p| {
                    array(p, state, false)
                }).or_else(|p| {
                    bool(p, state, false)
                }).or_else(|p| {
                    null(p, state, must_match)
                })
            })
        }

        fn string<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::string, pos, must_match, |pos, state| {
                pos.sequence(|p| {
                    p.match_string("\"").and_then(|p| {
                        p.repeat(|p| {
                            escape(p, state, false).or_else(|p| {
                                p.sequence(|p| {
                                    p.negate(|p| {
                                        state.lookahead(false, move |_| {
                                            p.lookahead(|p| {
                                                p.match_string("\"").or_else(|p| {
                                                    p.match_string("\\")
                                                })
                                            })
                                        })
                                    }).and_then(|p| {
                                        p.skip(1)
                                    })
                                })
                            })
                        })
                    }).and_then(|p| {
                        p.match_string("\"")
                    })
                })
            })
        }

        fn escape<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            pos.sequence(|p| {
                p.match_string("\\").and_then(|p| {
                    p.match_string("\"").or_else(|p| {
                        p.match_string("\\")
                    }).or_else(|p| {
                        p.match_string("/")
                    }).or_else(|p| {
                        p.match_string("b")
                    }).or_else(|p| {
                        p.match_string("f")
                    }).or_else(|p| {
                        p.match_string("n")
                    }).or_else(|p| {
                        p.match_string("r")
                    }).or_else(|p| {
                        p.match_string("t")
                    }).or_else(|p| {
                        unicode(p, state, must_match)
                    })
                })
            })
        }

        fn unicode<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            pos.sequence(|p| {
                p.match_string("u").and_then(|p| {
                    hex(p, state, must_match)
                }).and_then(|p| {
                    hex(p, state, must_match)
                }).and_then(|p| {
                    hex(p, state, must_match)
                })
            })
        }

        fn hex<I: Input>(pos: Position<I>, _: &mut ParserState<Rule, I>, _: bool)
            -> Result<Position<I>, Position<I>> {

            pos.match_range('0'..'9').or_else(|p| {
                p.match_range('a'..'f')
            }).or_else(|p| {
                p.match_range('A'..'F')
            })
        }

        fn number<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::number, pos, must_match, |pos, state| {
                pos.sequence(|p| {
                    p.optional(|p| {
                        p.match_string("-")
                    }).and_then(|p| {
                        int(p, state, must_match)
                    }).and_then(|p| {
                        p.optional(|p| {
                            p.sequence(|p| {
                                p.match_string(".").and_then(|p| {
                                    p.match_range('0'..'9')
                                }).and_then(|p| {
                                    p.repeat(|p| {
                                        p.match_range('0'..'9')
                                    })
                                }).and_then(|p| {
                                    p.optional(|p| {
                                        exp(p, state, false)
                                    })
                                }).or_else(|p| {
                                    exp(p, state, false)
                                })
                            })
                        })
                    })
                })
            })
        }

        fn int<I: Input>(pos: Position<I>, _: &mut ParserState<Rule, I>, _: bool)
            -> Result<Position<I>, Position<I>> {

            pos.match_string("0").or_else(|p| {
                p.sequence(|p| {
                    p.match_range('1'..'9').and_then(|p| {
                        p.repeat(|p| {
                            p.match_range('0'..'9')
                        })
                    })
                })
            })
        }

        fn exp<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            pos.sequence(|p| {
                p.match_string("E").or_else(|p| {
                    p.match_string("e")
                }).and_then(|p| {
                    p.optional(|p| {
                        p.match_string("+").or_else(|p| {
                            p.match_string("-")
                        })
                    })
                }).and_then(|p| {
                    int(p, state,  must_match)
                })
            })
        }

        fn bool<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::bool, pos, must_match, |pos, _| {
                pos.match_string("true").or_else(|p| {
                    p.match_string("false")
                })
            })
        }

        fn null<I: Input>(pos: Position<I>, state: &mut ParserState<Rule, I>, must_match: bool)
            -> Result<Position<I>, Position<I>> {

            state.rule(Rule::null, pos, must_match, |pos, _| {
                pos.match_string("null")
            })
        }

        fn skip<I: Input>(pos: Position<I>, _: &mut ParserState<Rule, I>, _: bool)
            -> Result<Position<I>, Position<I>> {

            pos.repeat(|p| {
                p.match_string(" ").or_else(|p| {
                    p.match_string("\t")
                }).or_else(|p| {
                    p.match_string("\r")
                }).or_else(|p| {
                    p.match_string("\n")
                })
            })
        }

        state(input, move |mut state| {
            if match rule {
                Rule::json    =>    json(state.start(), &mut state, true),
                Rule::object  =>  object(state.start(), &mut state, true),
                Rule::pair    =>    pair(state.start(), &mut state, true),
                Rule::array   =>   array(state.start(), &mut state, true),
                Rule::value   =>   value(state.start(), &mut state, true),
                Rule::string  =>  string(state.start(), &mut state, true),
                Rule::escape  =>  escape(state.start(), &mut state, true),
                Rule::unicode => unicode(state.start(), &mut state, true),
                Rule::hex     =>     hex(state.start(), &mut state, true),
                Rule::number  =>  number(state.start(), &mut state, true),
                Rule::int     =>     int(state.start(), &mut state, true),
                Rule::exp     =>     exp(state.start(), &mut state, true),
                Rule::bool    =>    bool(state.start(), &mut state, true),
                Rule::null    =>    null(state.start(), &mut state, true)
            }.is_err() {
                state.fail_with_attempts();
            }
        })
    }
}

enum Json<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Array(Vec<Json<'a>>),
    Object(HashMap<&'a str, Json<'a>>)
}

fn consume<'a, I: Input, S: TokenStream<Rule, I>>(stream: S) -> Json<'a> {
//    fn null<'a, I: Input, S: TokenStream<Rule, I>>(stream: S) -> Json<'a> {
//        stream.consume
//    }

    unimplemented!()
}

#[test]
fn null() {
    parses_to! {
        parser: JsonParser,
        input:  "null",
        rule:   Rule::null,
        tokens: [
            null(0, 4)
        ]
    };
}

#[test]
fn bool() {
    parses_to! {
        parser: JsonParser,
        input:  "false",
        rule:   Rule::bool,
        tokens: [
            bool(0, 5)
        ]
    };
}

#[test]
fn number_zero() {
    parses_to! {
        parser: JsonParser,
        input:  "0",
        rule:   Rule::number,
        tokens: [
            number(0, 1)
        ]
    };
}

#[test]
fn float() {
    parses_to! {
        parser: JsonParser,
        input:  "100.001",
        rule:   Rule::number,
        tokens: [
            number(0, 7)
        ]
    };
}

#[test]
fn float_with_exp() {
    parses_to! {
        parser: JsonParser,
        input:  "100.001E+100",
        rule:   Rule::number,
        tokens: [
            number(0, 12)
        ]
    };
}

#[test]
fn number_minus_zero() {
    parses_to! {
        parser: JsonParser,
        input:  "-0",
        rule:   Rule::number,
        tokens: [
            number(0, 2)
        ]
    };
}

#[test]
fn string_with_escapes() {
    parses_to! {
        parser: JsonParser,
        input:  "\"asd\\u0000\\\"\"",
        rule:   Rule::string,
        tokens: [
            string(0, 13)
        ]
    };
}

#[test]
fn array_empty() {
    parses_to! {
        parser: JsonParser,
        input:  "[ ]",
        rule:   Rule::array,
        tokens: [
            array(0, 3)
        ]
    };
}

#[test]
fn array() {
    parses_to! {
        parser: JsonParser,
        input:  "[0.0e1, false, null, \"a\", [0]]",
        rule:   Rule::array,
        tokens: [
            array(0, 30, [
                value( 1,  6, [ number(1,  6)]),
                value( 8, 13, [   bool(8, 13)]),
                value(15, 19, [  null(15, 19)]),
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
        input:  "{\"a\" : 3, \"b\" : [{}, 3]}",
        rule:   Rule::object,
        tokens: [
            object(0, 24, [
                pair(1, 8, [
                    string(1, 4),
                    value(7, 8, [number(7, 8)])
                ]),
                pair(10, 29, [
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
