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

use pest::error::Error;
use pest::iterators::Pairs;
use pest::{state, ParseResult, Parser, ParserState};

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expression,
    argument_list,
    function_call,
    callable_var,
    variable,
    symbol,
}

struct LeftRecursiveParser;

/// Represent these indirect recursive syntax.
///
///     function_call = {
///         callable_variable ~ argument_list
///     }
///
///     callable_variable = {
///         variable
///         |  symbol+
///         |  *function_call
///     }
///
impl Parser<Rule> for LeftRecursiveParser {
    fn parse(rule: Rule, input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        fn expression(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::expression, |s| {
                s.sequence(|s| {
                    variable(s)
                    .or_else(|s| function_call(s))
                })
            })
        }

        fn argument_list(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::argument_list, |s| {
                s.match_string("(")
                .and_then(|s| {
                    s.optional(|s| {
                            expression(s)
                            .and_then(|s| {
                                s.optional(|s| s.repeat(|s| {
                                    s.match_string(",")
                                    .and_then(|s| expression(s))
                                }))
                            })
                    })
                })
                .and_then(|s| s.match_string(")"))
            })
        }

        fn callable_var(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::callable_var, |s| {
                s.recursive(|s| function_call(s))
                .or_else(|s| variable(s))
                .or_else(|s| symbol(s))
            })
        }

        fn function_call(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::function_call, |s| {
                callable_var(s)
                .and_then(|s| argument_list(s))
            })
        }

        fn variable(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::variable, |s| {
                s.sequence(|s| {
                    s.match_string("$")
                    .and_then(|s| symbol(s))
                })
            })
        }

        fn symbol(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::symbol, |s| {
                s.match_range('A'..'z')
                .and_then(|s| s.repeat(|s| s.match_range('A'..'z')))
            })
        }

        state(input, |state| match rule {
            Rule::expression => expression(state),
            // For troubleshoot at first, but then a test
            Rule::argument_list => argument_list(state),
            _ => unreachable!(),
        })
    }
}

#[test]
fn variable() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "$variable",
        rule: Rule::expression,
        tokens: [
            expression(0, 9, [
                variable(0, 9, [
                    symbol(1, 9)
                ])
            ])
        ]
    };
}

#[test]
fn argument_list() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "($abc)",
        rule: Rule::argument_list,
        tokens: [
            argument_list(0, 6, [
                expression(1, 5, [
                    variable(1, 5, [
                        symbol(2, 5)
                    ])
                ])
            ])
        ]
    };

    parses_to! {
        parser: LeftRecursiveParser,
        input: "($abc,$cde)",
        rule: Rule::argument_list,
        tokens: [
            argument_list(0, 11, [
                expression(1, 5, [
                    variable(1, 5, [
                        symbol(2, 5)
                    ]),
                ]),
                expression(6, 10, [
                    variable(6, 10, [
                        symbol(7, 10)
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn function_call() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "func($arga)",
        rule: Rule::expression,
        tokens: [
            expression(0, 11, [
                function_call(0, 11, [
                    callable_var(0, 4, [
                        symbol(0, 4),
                    ]),
                    argument_list(4, 11, [
                        expression(5, 10, [
                            variable(5, 10, [
                                symbol(6, 10)
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn recursive_function_call() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "func()($arga)",
        rule: Rule::expression,
        tokens: [
            expression(0, 13, [
                function_call(0, 13, [
                    callable_var(0, 6, [
                        function_call(0, 6, [
                            callable_var(0, 4, [
                                symbol(0, 4),
                            ]),
                            argument_list(4, 6)
                        ]),
                    ]),
                    argument_list(6, 13, [
                        expression(7, 12, [
                            variable(7, 12, [
                                symbol(8, 12)
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}
