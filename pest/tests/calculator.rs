// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate pest;

use std::rc::Rc;

use pest::inputs::{Input, Position, StringInput};
use pest::iterators::{Pair, Pairs};
use pest::{Error, Parser, ParserState, state};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[allow(dead_code, non_camel_case_types)]
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
                    pos.sequence(|pos| {
                        primary(pos, state).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        plus(pos, state).or_else(|pos| {
                                            minus(pos, state)
                                        }).or_else(|pos| {
                                            times(pos, state)
                                        }).or_else(|pos| {
                                            divide(pos, state)
                                        }).or_else(|pos| {
                                            power(pos, state)
                                        }).and_then(|pos| {
                                            primary(pos, state)
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
            state.sequence(move |state| {
                pos.sequence(|pos| {
                    pos.match_string("(").and_then(|pos| {
                        expression(pos, state)
                    }).and_then(|pos| {
                        pos.match_string(")")
                    })
                })
            }).or_else(|pos| {
                number(pos, state)
            })
        }

        fn number<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::number, pos, |state, pos| {
                state.sequence(move |_| {
                    pos.sequence(|pos| {
                        pos.optional(|pos| {
                            pos.match_string("-")
                        }).and_then(|pos| {
                            pos.match_string("0").or_else(|pos| {
                                pos.sequence(|pos| {
                                    pos.match_range('1'..'9').and_then(|pos| {
                                        pos.repeat(|pos| {
                                            pos.match_range('0'..'9')
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
            state.rule(Rule::plus, pos, |_, pos| {
                pos.match_string("+")
            })
        }

        fn minus<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::minus, pos, |_, pos| {
                pos.match_string("-")
            })
        }

        fn times<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::times, pos, |_, pos| {
                pos.match_string("*")
            })
        }

        fn divide<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::divide, pos, |_, pos| {
                pos.match_string("/")
            })
        }

        fn power<I: Input>(
            pos: Position<I>,
            state: &mut ParserState<Rule, I>
        ) -> Result<Position<I>, Position<I>> {
            state.rule(Rule::power, pos, |_, pos| {
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

fn consume(pair: Pair<Rule, StringInput>, climber: &PrecClimber<Rule>) -> i32 {
    let primary = |pair| {
        consume(pair, climber)
    };
    let infix = |lhs: i32, op: Pair<Rule, StringInput>, rhs: i32| {
        match op.as_rule() {
            Rule::plus => lhs + rhs,
            Rule::minus => lhs - rhs,
            Rule::times => lhs * rhs,
            Rule::divide => lhs / rhs,
            Rule::power => lhs.pow(rhs as u32),
            _ => unreachable!()
        }
    };

    match pair.as_rule() {
        Rule::expression => climber.climb(pair.into_inner(), primary, infix),
        Rule::number => pair.into_span().as_str().parse().unwrap(),
        _ => unreachable!()
    }
}

#[test]
fn number() {
    parses_to! {
        parser: CalculatorParser,
        input: "-12",
        rule: Rule::expression,
        tokens: [
            expression(0, 3, [
                number(0, 3)
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
                expression(1, 6, [
                    expression(2, 5, [
                        number(2, 5)
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn expression() {
    parses_to! {
        parser: CalculatorParser,
        input: "-12+3*(4-9)^7^2",
        rule: Rule::expression,
        tokens: [
            expression(0, 15, [
                number(0, 3),
                plus(3, 4),
                number(4, 5),
                times(5, 6),
                expression(7, 10, [
                    number(7, 8),
                    minus(8, 9),
                    number(9, 10)
                ]),
                power(11, 12),
                number(12, 13),
                power(13, 14),
                number(14, 15)
            ])
        ]
    };
}

#[test]
fn prec_climb() {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right)
    ]);

    let pairs = CalculatorParser::parse_str(Rule::expression, "-12+3*(4-9)^3^2");
    assert_eq!(-5_859_387, consume(pairs.unwrap().next().unwrap(), &climber));
}
