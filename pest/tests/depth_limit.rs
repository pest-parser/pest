// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest::error::Error;
use pest::iterators::Pairs;
use pest::{state, ParseResult, Parser, ParserState};
use core::num::NonZeroUsize;

#[allow(dead_code, non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Rule {
    expression,
    add_expr,
    mul_expr,
    primary,
    number,
}

struct TestParser;

impl Parser<Rule> for TestParser {
    fn parse(rule: Rule, input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        fn expression(
            state: Box<ParserState<'_, Rule>>,
        ) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::expression, |s| {
                s.sequence(|s| {
                    s.start_of_input()
                        .and_then(add_expr)
                        .and_then(|s| s.end_of_input())
                })
            })
        }

        fn add_expr(
            state: Box<ParserState<'_, Rule>>,
        ) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::add_expr, |s| {
                s.sequence(|s| {
                    mul_expr(s).and_then(|s| {
                        s.repeat(|s| {
                            s.sequence(|s| {
                                s.match_string("+").and_then(mul_expr)
                            })
                        })
                    })
                })
            })
        }

        fn mul_expr(
            state: Box<ParserState<'_, Rule>>,
        ) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::mul_expr, |s| {
                s.sequence(|s| {
                    primary(s).and_then(|s| {
                        s.repeat(|s| {
                            s.sequence(|s| {
                                s.match_string("*").and_then(primary)
                            })
                        })
                    })
                })
            })
        }

        fn primary(
            state: Box<ParserState<'_, Rule>>,
        ) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::primary, |s| {
                number(s).or_else(|s| {
                    s.sequence(|s| {
                        s.match_string("(")
                            .and_then(add_expr)
                            .and_then(|s| s.match_string(")"))
                    })
                })
            })
        }

        fn number(
            state: Box<ParserState<'_, Rule>>,
        ) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::number, |s| {
                s.match_char_by(|c| c.is_ascii_digit())
                    .and_then(|s| {
                        s.repeat(|s| s.match_char_by(|c| c.is_ascii_digit()))
                    })
            })
        }

        state(input, |s| match rule {
            Rule::expression => expression(s),
            Rule::add_expr => add_expr(s),
            Rule::mul_expr => mul_expr(s),
            Rule::primary => primary(s),
            Rule::number => number(s),
        })
    }
}

#[test]
fn test_depth_limit_simple() {
    // Set a depth limit
    pest::set_depth_limit(Some(NonZeroUsize::new(10).unwrap()));
    
    // This should parse successfully - depth is less than 10
    let result = TestParser::parse(Rule::expression, "1");
    assert!(result.is_ok());
    
    // Reset depth limit
    pest::set_depth_limit(None);
}

#[test]
fn test_depth_limit_nested_parens() {
    // Set a very low depth limit
    pest::set_depth_limit(Some(NonZeroUsize::new(5).unwrap()));
    
    // Simple expression should fail with this very low limit
    // because even "1" requires multiple rule invocations:
    // expression -> add_expr -> mul_expr -> primary -> number
    let result = TestParser::parse(Rule::expression, "1");
    
    match result {
        Ok(_) => {
            panic!("Expected depth limit error with very low limit");
        }
        Err(e) => {
            let error_msg = format!("{}", e);
            eprintln!("Error message: {}", error_msg);
            eprintln!("Error variant: {:?}", e.variant);
            // Check if it's a custom error with depth limit message
            if let pest::error::ErrorVariant::CustomError { message } = &e.variant {
                assert!(message.contains("depth limit reached") || message.contains("call limit reached"), 
                        "Expected depth/call limit error, got: {}", message);
            } else {
                panic!("Expected CustomError variant, got: {:?}", e.variant);
            }
        }
    }
    
    // Reset depth limit
    pest::set_depth_limit(None);
}

#[test]
fn test_depth_limit_allows_simple_parse() {
    // Set a reasonable depth limit
    pest::set_depth_limit(Some(NonZeroUsize::new(50).unwrap()));
    
    // Simple expression should work with reasonable limit
    let result = TestParser::parse(Rule::expression, "1+2");
    assert!(result.is_ok());
    
    // Reset depth limit
    pest::set_depth_limit(None);
}

#[test]
fn test_no_depth_limit() {
    // Make sure no limit allows parsing
    pest::set_depth_limit(None);
    
    // Even deeply nested expressions should work without a limit
    let nested = "((((((1))))))";
    let result = TestParser::parse(Rule::expression, nested);
    assert!(result.is_ok());
}

#[test]
fn test_depth_limit_reset() {
    // Set a limit, then remove it
    pest::set_depth_limit(Some(NonZeroUsize::new(5).unwrap()));
    pest::set_depth_limit(None);
    
    // Should work after reset
    let result = TestParser::parse(Rule::expression, "((((((1))))))");
    assert!(result.is_ok());
}
