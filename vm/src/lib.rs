// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

extern crate pest;
extern crate pest_meta;

use pest::error::Error;
use pest::iterators::Pairs;
use pest::unicode;
use pest::{Atomicity, ParseResult, ParserState};
use pest_meta::ast::RuleType;
use pest_meta::optimizer::{OptimizedExpr, OptimizedRule};

use std::collections::HashMap;

mod macros;

pub struct Vm {
    rules: HashMap<String, OptimizedRule>
}

impl Vm {
    pub fn new(rules: Vec<OptimizedRule>) -> Vm {
        let rules = rules.into_iter().map(|r| (r.name.clone(), r)).collect();
        Vm { rules }
    }

    pub fn parse<'a, 'i>(
        &'a self,
        rule: &'a str,
        input: &'i str
    ) -> Result<Pairs<'i, &str>, Error<&str>> {
        pest::state(input, |state| self.parse_rule(rule, state))
    }

    fn parse_rule<'a, 'i>(
        &'a self,
        rule: &'a str,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match rule {
            "ANY" => return state.skip(1),
            "EOI" => return state.rule("EOI", |state| state.end_of_input()),
            "SOI" => return state.start_of_input(),
            "PEEK" => return state.stack_peek(),
            "PEEK_ALL" => return state.stack_match_peek(),
            "POP" => return state.stack_pop(),
            "POP_ALL" => return state.stack_match_pop(),
            "DROP" => return state.stack_drop(),
            "ASCII_DIGIT" => return state.match_range('0'..'9'),
            "ASCII_NONZERO_DIGIT" => return state.match_range('1'..'9'),
            "ASCII_BIN_DIGIT" => return state.match_range('0'..'1'),
            "ASCII_OCT_DIGIT" => return state.match_range('0'..'7'),
            "ASCII_HEX_DIGIT" => {
                return state
                    .match_range('0'..'9')
                    .or_else(|state| state.match_range('a'..'f'))
                    .or_else(|state| state.match_range('A'..'F'))
            }
            "ASCII_ALPHA_LOWER" => return state.match_range('a'..'z'),
            "ASCII_ALPHA_UPPER" => return state.match_range('A'..'Z'),
            "ASCII_ALPHA" => {
                return state
                    .match_range('a'..'z')
                    .or_else(|state| state.match_range('A'..'Z'))
            }
            "ASCII_ALPHANUMERIC" => {
                return state
                    .match_range('a'..'z')
                    .or_else(|state| state.match_range('A'..'Z'))
                    .or_else(|state| state.match_range('0'..'9'))
            }
            "ASCII" => return state.match_range('\x00'..'\x7f'),
            "NEWLINE" => {
                return state
                    .match_string("\n")
                    .or_else(|state| state.match_string("\r\n"))
                    .or_else(|state| state.match_string("\r"))
            }
            _ => ()
        };

        if let Some(rule) = self.rules.get(rule) {
            if &rule.name == "WHITESPACE" || &rule.name == "COMMENT" {
                match rule.ty {
                    RuleType::Normal => state.rule(&rule.name, |state| {
                        state.atomic(Atomicity::Atomic, |state| {
                            self.parse_expr(&rule.expr, state)
                        })
                    }),
                    RuleType::Silent => state.atomic(Atomicity::Atomic, |state| {
                        self.parse_expr(&rule.expr, state)
                    }),
                    RuleType::Atomic => state.rule(&rule.name, |state| {
                        state.atomic(Atomicity::Atomic, |state| {
                            self.parse_expr(&rule.expr, state)
                        })
                    }),
                    RuleType::CompoundAtomic => state.atomic(Atomicity::CompoundAtomic, |state| {
                        state.rule(&rule.name, |state| self.parse_expr(&rule.expr, state))
                    }),
                    RuleType::NonAtomic => state.atomic(Atomicity::Atomic, |state| {
                        state.rule(&rule.name, |state| self.parse_expr(&rule.expr, state))
                    })
                }
            } else {
                match rule.ty {
                    RuleType::Normal => {
                        state.rule(&rule.name, move |state| self.parse_expr(&rule.expr, state))
                    }
                    RuleType::Silent => self.parse_expr(&rule.expr, state),
                    RuleType::Atomic => state.rule(&rule.name, move |state| {
                        state.atomic(Atomicity::Atomic, move |state| {
                            self.parse_expr(&rule.expr, state)
                        })
                    }),
                    RuleType::CompoundAtomic => {
                        state.atomic(Atomicity::CompoundAtomic, move |state| {
                            state.rule(&rule.name, |state| self.parse_expr(&rule.expr, state))
                        })
                    }
                    RuleType::NonAtomic => state.atomic(Atomicity::NonAtomic, move |state| {
                        state.rule(&rule.name, |state| self.parse_expr(&rule.expr, state))
                    })
                }
            }
        } else {
            if let Some(property) = unicode::by_name(rule) {
                // std::boxed::Box<dyn std::ops::Fn(char) -> bool> is not FnOnce(char)->bool
                #[cfg_attr(feature = "cargo-clippy", allow(redundant_closure))]
                return state.match_char_by(|c| property(c));
            }

            panic!("undefined rule {}", rule);
        }
    }

    fn parse_expr<'a, 'i>(
        &'a self,
        expr: &'a OptimizedExpr,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match *expr {
            OptimizedExpr::Str(ref string) => state.match_string(string),
            OptimizedExpr::Insens(ref string) => state.match_insensitive(string),
            OptimizedExpr::Range(ref start, ref end) => {
                let start = start.chars().next().expect("empty char literal");
                let end = end.chars().next().expect("empty char literal");

                state.match_range(start..end)
            }
            OptimizedExpr::Ident(ref name) => self.parse_rule(name, state),
            OptimizedExpr::PosPred(ref expr) => {
                state.lookahead(true, |state| self.parse_expr(expr, state))
            }
            OptimizedExpr::NegPred(ref expr) => {
                state.lookahead(false, |state| self.parse_expr(expr, state))
            }
            OptimizedExpr::Seq(ref lhs, ref rhs) => state.sequence(|state| {
                self.parse_expr(lhs, state)
                    .and_then(|state| self.skip(state))
                    .and_then(|state| self.parse_expr(rhs, state))
            }),
            OptimizedExpr::Choice(ref lhs, ref rhs) => self
                .parse_expr(lhs, state)
                .or_else(|state| self.parse_expr(rhs, state)),
            OptimizedExpr::Opt(ref expr) => state.optional(|state| self.parse_expr(expr, state)),
            OptimizedExpr::Rep(ref expr) => state.sequence(|state| {
                state.optional(|state| {
                    self.parse_expr(expr, state).and_then(|state| {
                        state.repeat(|state| {
                            state.sequence(|state| {
                                self.skip(state)
                                    .and_then(|state| self.parse_expr(expr, state))
                            })
                        })
                    })
                })
            }),
            OptimizedExpr::Push(ref expr) => state.stack_push(|state| self.parse_expr(expr, state)),
            OptimizedExpr::Skip(ref strings) => state.skip_until(
                &strings
                    .iter()
                    .map(|state| state.as_str())
                    .collect::<Vec<&str>>()
            ),
            OptimizedExpr::RestoreOnErr(ref expr) => {
                state.restore_on_err(|state| self.parse_expr(expr, state))
            }
        }
    }

    fn skip<'a, 'i>(
        &'a self,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match (
            self.rules.contains_key("WHITESPACE"),
            self.rules.contains_key("COMMENT")
        ) {
            (false, false) => Ok(state),
            (true, false) => {
                if state.atomicity() == Atomicity::NonAtomic {
                    state.repeat(|state| self.parse_rule("WHITESPACE", state))
                } else {
                    Ok(state)
                }
            }
            (false, true) => {
                if state.atomicity() == Atomicity::NonAtomic {
                    state.repeat(|state| self.parse_rule("COMMENT", state))
                } else {
                    Ok(state)
                }
            }
            (true, true) => {
                if state.atomicity() == Atomicity::NonAtomic {
                    state.sequence(|state| {
                        state
                            .repeat(|state| self.parse_rule("WHITESPACE", state))
                            .and_then(|state| {
                                state.repeat(|state| {
                                    state.sequence(|state| {
                                        self.parse_rule("COMMENT", state).and_then(|state| {
                                            state.repeat(|state| {
                                                self.parse_rule("WHITESPACE", state)
                                            })
                                        })
                                    })
                                })
                            })
                    })
                } else {
                    Ok(state)
                }
            }
        }
    }
}
