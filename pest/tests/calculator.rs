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
    expression,
    primary,
    number,
    plus,
    minus,
    times,
    divide,
    power
}

struct CalculatorParser;

impl Parser<Rule> for CalculatorParser {
    fn parse<I: Input>(rule: Rule, input: Rc<I>) -> Result<Pairs<Rule, I>, Error<Rule, I>> {
        fn expression<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::expression, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        primary(p, state).and_then(|p| {
                            p.repeat(|p| {
                                state.sequence(move |state| {
                                    p.sequence(|p| {
                                        plus(p, state).or_else(|p| {
                                            minus(p, state)
                                        }).or_else(|p| {
                                            times(p, state)
                                        }).or_else(|p| {
                                            divide(p, state)
                                        }).or_else(|p| {
                                            power(p, state)
                                        }).and_then(|p| {
                                            primary(p, state)
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn primary<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::primary, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.match_string("(").and_then(|p| {
                            expression(p, state)
                        }).and_then(|p| {
                            p.match_string(")")
                        })
                    })
                }).or_else(|p| {
                    number(p, state)
                })
            })
        }

        fn number<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::number, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|p| {
                        p.optional(|p| {
                            p.match_string("-")
                        }).and_then(|p| {
                            p.match_string("0").or_else(|p| {
                                p.sequence(|p| {
                                    p.match_range('1'..'9').and_then(|p| {
                                        p.repeat(|p| {
                                            p.match_range('0'..'9')
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn plus<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::plus, pos, |state, pos| {
                pos.match_string("+")
            })
        }

        fn minus<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::minus, pos, |state, pos| {
                pos.match_string("-")
            })
        }

        fn times<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::times, pos, |state, pos| {
                pos.match_string("*")
            })
        }

        fn divide<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::divide, pos, |state, pos| {
                pos.match_string("/")
            })
        }

        fn power<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::power, pos, |state, pos| {
                pos.match_string("^")
            })
        }

        state(input, move |mut state, pos| {
            match rule {
                Rule::expression => expression(pos, &mut state),
                Rule::primary => primary(pos, &mut state),
                Rule::number => number(pos, &mut state),
                Rule::plus => plus(pos, &mut state),
                Rule::minus => minus(pos, &mut state),
                Rule::times => times(pos, &mut state),
                Rule::divide => divide(pos, &mut state),
                Rule::power => power(pos, &mut state)
            }
        })
    }
}

//fn consume(pair: Pair<Rule, StringInput>) -> Json {
//    fn value(pair: Pair<Rule, StringInput>) -> Json {
//        let pair = pair.consume().next().unwrap();
//
//        match pair.rule() {
//            Rule::null => Json::Null,
//            Rule::bool => {
//                match pair.span().capture() {
//                    "false" => Json::Bool(false),
//                    "true" => Json::Bool(true),
//                    _ => unreachable!()
//                }
//            }
//            Rule::number => {
//                Json::Number(pair.span().capture().parse().unwrap())
//            }
//            Rule::string => {
//                Json::String(pair.span())
//            }
//            Rule::array => {
//                Json::Array(pair.consume().map(|p| value(p)).collect())
//            }
//            Rule::object => {
//                let pairs = pair.consume().map(|p| {
//                    let mut pair = p.consume();
//
//                    let key = pair.next().unwrap().span();
//                    let value = value(pair.next().unwrap());
//
//                    (key, value)
//                });
//
//                Json::Object(pairs.collect())
//            }
//            _ => unreachable!()
//        }
//    }
//
//    value(pair)
//}

#[test]
fn number() {
    parses_to! {
        parser: CalculatorParser,
        input: "-12",
        rule: Rule::expression,
        tokens: [
            expression(0, 3, [
                primary(0, 3, [
                    number(0, 3)
                ])
            ])
        ]
    };
}

#[test]
fn parens() {
    parses_to! {
        parser: CalculatorParser,
        input: "((-12))",
        rule: Rule::expression,
        tokens: [
            expression(0, 7, [
                primary(0, 7, [
                    expression(1, 6, [
                        primary(1, 6, [
                            expression(2, 5, [
                                primary(2, 5, [
                                    number(2, 5)
                                ])
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}
