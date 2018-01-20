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

use pest::{state, Error, Parser, ParserState};
use pest::Position;
use pest::iterators::{Pair, Pairs};
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
    modulus,
    power
}

struct CalculatorParser;

impl Parser<Rule> for CalculatorParser {
    fn parse(rule: Rule, input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        fn expression<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::expression, pos, |state, pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        primary(pos, state).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        plus(pos, state)
                                            .or_else(|pos| minus(pos, state))
                                            .or_else(|pos| times(pos, state))
                                            .or_else(|pos| divide(pos, state))
                                            .or_else(|pos| modulus(pos, state))
                                            .or_else(|pos| power(pos, state))
                                            .and_then(|pos| primary(pos, state))
                                    })
                                })
                            })
                        })
                    })
                })
            })
        }

        fn primary<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state
                .sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_string("(")
                            .and_then(|pos| expression(pos, state))
                            .and_then(|pos| pos.match_string(")"))
                    })
                })
                .or_else(|pos| number(pos, state))
        }

        fn number<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::number, pos, |state, pos| {
                state.sequence(move |_| {
                    pos.sequence(|pos| {
                        pos.optional(|pos| pos.match_string("-")).and_then(|pos| {
                            pos.match_string("0").or_else(|pos| {
                                pos.sequence(|pos| {
                                    pos.match_range('1'..'9')
                                        .and_then(|pos| pos.repeat(|pos| pos.match_range('0'..'9')))
                                })
                            })
                        })
                    })
                })
            })
        }

        fn plus<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::plus, pos, |_, pos| pos.match_string("+"))
        }

        fn minus<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::minus, pos, |_, pos| pos.match_string("-"))
        }

        fn times<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::times, pos, |_, pos| pos.match_string("*"))
        }

        fn divide<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::divide, pos, |_, pos| pos.match_string("/"))
        }

        fn modulus<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::modulus, pos, |_, pos| pos.match_string("%"))
        }

        fn power<'i>(
            pos: Position<'i>,
            state: &mut ParserState<'i, Rule>
        ) -> Result<Position<'i>, Position<'i>> {
            state.rule(Rule::power, pos, |_, pos| pos.match_string("^"))
        }

        state(input, move |mut state, pos| match rule {
            Rule::expression => expression(pos, &mut state),
            Rule::primary => primary(pos, &mut state),
            Rule::number => number(pos, &mut state),
            Rule::plus => plus(pos, &mut state),
            Rule::minus => minus(pos, &mut state),
            Rule::times => times(pos, &mut state),
            Rule::divide => divide(pos, &mut state),
            Rule::modulus => modulus(pos, &mut state),
            Rule::power => power(pos, &mut state)
        })
    }
}

fn consume<'i>(pair: Pair<'i, Rule>, climber: &PrecClimber<Rule>) -> i32 {
    let primary = |pair| consume(pair, climber);
    let infix = |lhs: i32, op: Pair<Rule>, rhs: i32| match op.as_rule() {
        Rule::plus => lhs + rhs,
        Rule::minus => lhs - rhs,
        Rule::times => lhs * rhs,
        Rule::divide => lhs / rhs,
        Rule::modulus => lhs % rhs,
        Rule::power => lhs.pow(rhs as u32),
        _ => unreachable!()
    };

    match pair.as_rule() {
        Rule::expression => climber.climb(pair.into_inner(), primary, infix),
        Rule::number => pair.as_str().parse().unwrap(),
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
        Operator::new(Rule::times, Assoc::Left) | Operator::new(Rule::divide, Assoc::Left)
            | Operator::new(Rule::modulus, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right),
    ]);

    let pairs = CalculatorParser::parse(Rule::expression, "-12+3*(4-9)^3^2/9%7381");
    assert_eq!(-1_525, consume(pairs.unwrap().next().unwrap(), &climber));
}
