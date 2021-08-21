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


/// Represent this simple recursive syntax.
///
///     function_call = {
///         (  variable
///         |  symbol+
///         |  *function_call
///         ) ~ argument_list
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
                .and_then(|s| s.match_string(")"))
            })
        }

        fn function_call(state: Box<ParserState<Rule>>) -> ParseResult<Box<ParserState<Rule>>> {
            state.rule(Rule::function_call, |s| {
                s.recursive(|s| function_call(s))
                .or_else(|s| variable(s))
                .or_else(|s| symbol(s))
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
            state.rule(Rule::symbol, |s| s.repeat(|s| s.match_range('A'..'z')))
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
        input: "()",
        rule: Rule::argument_list,
        tokens: [
            argument_list(0, 2)
        ]
    };
}

#[test]
fn function_call() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "func()",
        rule: Rule::expression,
        tokens: [
            expression(0, 6, [
                function_call(0, 6, [
                    symbol(0, 4),
                    argument_list(4, 6)
                ])
            ])
        ]
    };
}

#[test]
fn recursive_function_call() {
    parses_to! {
        parser: LeftRecursiveParser,
        input: "func()()",
        rule: Rule::expression,
        tokens: [
            expression(0, 8, [
                function_call(0, 8, [
                    function_call(0, 6, [
                        symbol(0, 4),
                        argument_list(4, 6)
                    ]),
                    argument_list(6, 8)
                ])
            ])
        ]
    };
}
