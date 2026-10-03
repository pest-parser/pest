// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use core::num::NonZeroUsize;
use pest::error::Error;
use pest::iterators::Pairs;
use pest::{state, ParseResult, Parser, ParserState};
use std::sync::{Mutex, MutexGuard};

static CALL_LIMIT_LOCK: Mutex<()> = Mutex::new(());

struct CallLimitGuard {
    _lock: MutexGuard<'static, ()>,
}

impl CallLimitGuard {
    fn new(limit: Option<NonZeroUsize>) -> Self {
        let lock = CALL_LIMIT_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        pest::set_call_limit(limit);
        Self { _lock: lock }
    }
}

impl Drop for CallLimitGuard {
    fn drop(&mut self) {
        pest::set_call_limit(None);
    }
}

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
    fn parse(rule: Rule, input: &str) -> Result<Pairs<'_, Rule>, Error<Rule>> {
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

        fn add_expr(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::add_expr, |s| {
                s.sequence(|s| {
                    mul_expr(s).and_then(|s| {
                        s.repeat(|s| s.sequence(|s| s.match_string("+").and_then(mul_expr)))
                    })
                })
            })
        }

        fn mul_expr(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::mul_expr, |s| {
                s.sequence(|s| {
                    primary(s).and_then(|s| {
                        s.repeat(|s| s.sequence(|s| s.match_string("*").and_then(primary)))
                    })
                })
            })
        }

        fn primary(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
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

        fn number(state: Box<ParserState<'_, Rule>>) -> ParseResult<Box<ParserState<'_, Rule>>> {
            state.rule(Rule::number, |s| {
                s.match_char_by(|c| c.is_ascii_digit())
                    .and_then(|s| s.repeat(|s| s.match_char_by(|c| c.is_ascii_digit())))
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
    // Set a recursion depth limit
    let _guard = CallLimitGuard::new(NonZeroUsize::new(10));

    // This should parse successfully - depth is less than 10
    let result = TestParser::parse(Rule::expression, "1");
    assert!(result.is_ok());
}

#[test]
fn test_depth_limit_nested_parens() {
    let _guard = CallLimitGuard::new(NonZeroUsize::new(4));

    // Simple expression should fail with this very low limit
    let result = TestParser::parse(Rule::expression, "1");

    match result {
        Ok(_) => {
            panic!("Expected call limit error with very low limit");
        }
        Err(e) => {
            let _error_msg = format!("{e}");
            // Check specifically for call limit error
            if let pest::error::ErrorVariant::CustomError { message } = &e.variant {
                assert_eq!(
                    message, "call limit reached",
                    "Expected call limit error, got: {message}"
                );
            } else {
                panic!(
                    "Expected CustomError variant with call limit, got: {:?}",
                    e.variant
                );
            }
        }
    }
}

#[test]
fn test_depth_limit_allows_simple_parse() {
    // Set a reasonable recursion depth limit
    let _guard = CallLimitGuard::new(NonZeroUsize::new(50));

    // Simple expression should work with reasonable limit
    let result = TestParser::parse(Rule::expression, "1+2");
    assert!(result.is_ok());
}

#[test]
fn test_no_depth_limit() {
    // Make sure no limit allows parsing
    let _guard = CallLimitGuard::new(None);

    // Even deeply nested expressions should work without a limit
    let nested = "((((((1))))))";
    let result = TestParser::parse(Rule::expression, nested);
    assert!(result.is_ok());
}

#[test]
fn test_depth_limit_reset() {
    // Set a limit, then remove it
    let _guard = CallLimitGuard::new(NonZeroUsize::new(5));
    pest::set_call_limit(None);

    // Should work after reset
    let result = TestParser::parse(Rule::expression, "((((((1))))))");
    assert!(result.is_ok());
}

/// Checks depth rejection with the hand-written parser. The exact generated
/// grammar and fixed-stack regression are covered by pest_derive's tests.
#[test]
fn test_prevents_stack_overflow_from_issue() {
    let _guard = CallLimitGuard::new(NonZeroUsize::new(50));

    // Create a deeply nested expression with 30 levels of nesting
    // This would need more than 50 depth
    let mut deeply_nested = String::new();
    let nesting_depth = 30;

    // Add opening parens
    for _ in 0..nesting_depth {
        deeply_nested.push('(');
    }
    deeply_nested.push('1');
    // Add closing parens
    for _ in 0..nesting_depth {
        deeply_nested.push(')');
    }

    let result = TestParser::parse(Rule::expression, &deeply_nested);

    // Should fail with call limit reached (recursion depth limit), not stack overflow
    assert!(result.is_err());
    if let Err(e) = result {
        let error_msg = format!("{e}");
        assert!(
            error_msg.contains("call limit reached"),
            "Expected call limit error, got: {error_msg}"
        );
    }
}
