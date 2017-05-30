// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use std::collections::HashMap;
use std::rc::Rc;

use pest::inputs::{Input, Position, Span, StringInput};
use pest::iterators::{Pair, Pairs};
use pest::{Error, Parser, ParserState, state};

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
    fn parse<I: Input>(rule: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
        fn json<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            value(pos, state)
        }

        fn object<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::object, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_string("{").and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pair(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        pos.match_string(",").and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            pair(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            pos.match_string("}")
                        })
                    })
                }).or_else(|pos| {
                    state.sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("{").and_then(|pos| {
                                skip(pos, state)
                            }).and_then(|pos| {
                                pos.match_string("}")
                            })
                        })
                    })
                })
            })
        }

        fn pair<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::pair, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        string(pos, state).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.match_string(":")
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            value(pos, state)
                        })
                    })
                })
            })
        }

        fn array<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::array, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_string("[").and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            value(pos, state)
                        }).and_then(|pos| {
                            skip(pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        pos.match_string(",").and_then(|pos| {
                                            skip(pos, state)
                                        }).and_then(|pos| {
                                            value(pos, state)
                                        }).and_then(|pos| {
                                            skip(pos, state)
                                        })
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            pos.match_string("]")
                        })
                    })
                }).or_else(|pos| {
                    state.sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.match_string("[").and_then(|pos| {
                                skip(pos, state)
                            }).and_then(|pos| {
                                pos.match_string("]")
                            })
                        })
                    })
                })
            })
        }

        fn value<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::value, pos, |state, pos| {
                string(pos, state).or_else(|pos| {
                    number(pos, state)
                }).or_else(|pos| {
                    object(pos, state)
                }).or_else(|pos| {
                    array(pos, state)
                }).or_else(|pos| {
                    bool(pos, state)
                }).or_else(|pos| {
                    null(pos, state)
                })
            })
        }

        fn string<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::string, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.match_string("\"").and_then(|pos| {
                        pos.repeat(|pos| {
                            escape(pos, state).or_else(|pos| {
                                pos.sequence(|pos| {
                                    state.lookahead(false, move |_| {
                                        pos.lookahead(false, |pos| {
                                            pos.match_string("\"").or_else(|pos| {
                                                pos.match_string("\\")
                                            })
                                        })
                                    }).and_then(|pos| {
                                        pos.skip(1)
                                    })
                                })
                            })
                        })
                    }).and_then(|pos| {
                        pos.match_string("\"")
                    })
                })
            })
        }

        fn escape<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("\\").and_then(|pos| {
                    pos.match_string("\"").or_else(|pos| {
                        pos.match_string("\\")
                    }).or_else(|pos| {
                        pos.match_string("/")
                    }).or_else(|pos| {
                        pos.match_string("b")
                    }).or_else(|pos| {
                        pos.match_string("f")
                    }).or_else(|pos| {
                        pos.match_string("n")
                    }).or_else(|pos| {
                        pos.match_string("r")
                    }).or_else(|pos| {
                        pos.match_string("t")
                    }).or_else(|pos| {
                        unicode(pos, state)
                    })
                })
            })
        }

        fn unicode<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("u").and_then(|pos| {
                    hex(pos, state)
                }).and_then(|pos| {
                    hex(pos, state)
                }).and_then(|pos| {
                    hex(pos, state)
                })
            })
        }

        fn hex<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_range('0'..'9').or_else(|pos| {
                pos.match_range('a'..'f')
            }).or_else(|pos| {
                pos.match_range('A'..'F')
            })
        }

        fn number<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::number, pos, |state, pos| {
                pos.sequence(|pos| {
                    pos.optional(|pos| {
                        pos.match_string("-")
                    }).and_then(|pos| {
                        int(pos, state)
                    }).and_then(|pos| {
                        pos.optional(|pos| {
                            pos.sequence(|pos| {
                                pos.match_string(".").and_then(|pos| {
                                    pos.match_range('0'..'9')
                                }).and_then(|pos| {
                                    pos.repeat(|pos| {
                                        pos.match_range('0'..'9')
                                    })
                                }).and_then(|pos| {
                                    pos.optional(|pos| {
                                        exp(pos, state)
                                    })
                                }).or_else(|pos| {
                                    exp(pos, state)
                                })
                            })
                        })
                    })
                })
            })
        }

        fn int<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.match_string("0").or_else(|pos| {
                pos.sequence(|pos| {
                    pos.match_range('1'..'9').and_then(|pos| {
                        pos.repeat(|pos| {
                            pos.match_range('0'..'9')
                        })
                    })
                })
            })
        }

        fn exp<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.sequence(|pos| {
                pos.match_string("E").or_else(|pos| {
                    pos.match_string("e")
                }).and_then(|pos| {
                    pos.optional(|pos| {
                        pos.match_string("+").or_else(|pos| {
                            pos.match_string("-")
                        })
                    })
                }).and_then(|pos| {
                    int(pos, state)
                })
            })
        }

        fn bool<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::bool, pos, |_, pos| {
                pos.match_string("true").or_else(|pos| {
                    pos.match_string("false")
                })
            })
        }

        fn null<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::null, pos, |_, pos| {
                pos.match_string("null")
            })
        }

        fn skip<I: Input>(
            pos: Position<I>,
            _: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            pos.repeat(|pos| {
                pos.match_string(" ").or_else(|pos| {
                    pos.match_string("\t")
                }).or_else(|pos| {
                    pos.match_string("\r")
                }).or_else(|pos| {
                    pos.match_string("\n")
                })
            })
        }

        state(input, move |mut state, pos| {
            match rule {
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
            }
        })
    }
}

#[derive(Debug, PartialEq)]
enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(Span<StringInput>),
    Array(Vec<Json>),
    Object(HashMap<Span<StringInput>, Json>)
}

fn consume(pair: Pair<Rule, StringInput>) -> Json {
    fn value(pair: Pair<Rule, StringInput>) -> Json {
        let pair = pair.consume().next().unwrap();

        match pair.rule() {
            Rule::null => Json::Null,
            Rule::bool => {
                match pair.span().capture() {
                    "false" => Json::Bool(false),
                    "true" => Json::Bool(true),
                    _ => unreachable!()
                }
            }
            Rule::number => {
                Json::Number(pair.span().capture().parse().unwrap())
            }
            Rule::string => {
                Json::String(pair.span())
            }
            Rule::array => {
                Json::Array(pair.consume().map(|pos| value(pos)).collect())
            }
            Rule::object => {
                let pairs = pair.consume().map(|pos| {
                    let mut pair = pos.consume();

                    let key = pair.next().unwrap().span();
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
    let input = Rc::new(StringInput::new("{\"a\": [null, true, 3.4]}".to_owned()));
    let start = Position::from_start(input.clone()).skip(1).unwrap();
    let end = start.clone().skip(3).unwrap();
    let span = start.span(end);

    let mut pairs = HashMap::new();
    pairs.insert(span, Json::Array(vec![
        Json::Null,
        Json::Bool(true),
        Json::Number(3.4)
    ]));

    let ast = consume(JsonParser::parse(Rule::json, input.clone()).unwrap().next().unwrap());

    assert_eq!(ast, Json::Object(pairs));
}
