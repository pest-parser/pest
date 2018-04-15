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

use pest::{Atomicity, Error, ParseResult, ParserState};
use pest::iterators::Pairs;
use pest_meta::ast::{Expr, Rule, RuleType};

use std::collections::HashMap;

mod macros;

pub struct Vm {
    rules: HashMap<String, Rule>
}

impl Vm {
    pub fn new(rules: Vec<Rule>) -> Vm {
        let rules = rules.into_iter().map(|r| (r.name.clone(), r)).collect();
        Vm { rules }
    }

    pub fn parse<'a, 'i>(
        &'a self,
        rule: &'a str,
        input: &'i str
    ) -> Result<Pairs<'i, &str>, Error<'i, &str>> {
        pest::state(input, |state| self.parse_rule(rule, state))
    }

    fn parse_rule<'a, 'i>(
        &'a self,
        rule: &'a str,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match rule {
            "any" => return state.skip(1),
            "eoi" => return state.rule("eoi", |state| state.end_of_input()),
            "soi" => return state.start_of_input(),
            "peek" => return state.stack_peek(),
            "pop" => return state.stack_pop(),
            "drop" => return state.stack_drop(),
            _ => ()
        };

        if let Some(rule) = self.rules.get(rule) {
            if &rule.name == "whitespace" || &rule.name == "comment" {
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
            panic!("undefined rule {}", rule);
        }
    }

    fn parse_expr<'a, 'i>(
        &'a self,
        expr: &'a Expr,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match *expr {
            Expr::Str(ref string) => state.match_string(string),
            Expr::Insens(ref string) => state.match_insensitive(string),
            Expr::Range(ref start, ref end) => {
                let start = start.chars().next().expect("empty char literal");
                let end = end.chars().next().expect("empty char literal");

                state.match_range(start..end)
            }
            Expr::Ident(ref name) => self.parse_rule(name, state),
            Expr::PosPred(ref expr) => {
                state.lookahead(true, |state| self.parse_expr(&*expr, state))
            }
            Expr::NegPred(ref expr) => {
                state.lookahead(false, |state| self.parse_expr(&*expr, state))
            }
            Expr::Seq(ref lhs, ref rhs) => state.sequence(|state| {
                self.parse_expr(&*lhs, state)
                    .and_then(|state| self.skip(state))
                    .and_then(|state| self.parse_expr(&*rhs, state))
            }),
            Expr::Choice(ref lhs, ref rhs) => self.parse_expr(&*lhs, state)
                .or_else(|state| self.parse_expr(&*rhs, state)),
            Expr::Opt(ref expr) => state.optional(|state| self.parse_expr(&*expr, state)),
            Expr::Rep(ref expr) => self.repeat(expr, None, None, state),
            Expr::RepOnce(ref expr) => self.repeat(expr, Some(1), None, state),
            Expr::RepExact(ref expr, num) => self.repeat(expr, Some(num), Some(num), state),
            Expr::RepMin(ref expr, min) => self.repeat(expr, Some(min), None, state),
            Expr::RepMax(ref expr, max) => self.repeat(expr, None, Some(max), state),
            Expr::RepMinMax(ref expr, min, max) => self.repeat(expr, Some(min), Some(max), state),
            Expr::Push(ref expr) => state.stack_push(|state| self.parse_expr(&*expr, state)),
            Expr::Skip(ref strings) => {
                state.skip_until(&strings.iter().map(|state| state.as_str()).collect::<Vec<&str>>())
            },
            Expr::RestoreOnErr(ref expr) => {
                state.restore_on_err(|state| self.parse_expr(&*expr, state))
            }
        }
    }

    fn repeat<'a, 'i>(
        &'a self,
        expr: &'a Expr,
        min: Option<u32>,
        max: Option<u32>,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        state.sequence(|state| {
            let mut result = match min {
                Some(min) if min > 0 => {
                    let mut result = self.parse_expr(&*expr, state);

                    for _ in 2..min + 1 {
                        result = result.and_then(|state| {
                            self.skip(state)
                                .and_then(|state| self.parse_expr(&*expr, state))
                        });
                    }

                    result
                }
                _ => state.optional(|state| self.parse_expr(&*expr, state))
            };

            let mut times = 1;

            loop {
                if let Some(max) = max {
                    if times >= max {
                        return result;
                    }
                }

                let initial_result_is_err = result.is_err();

                let current = result.and_then(|state| {
                    state.lookahead(true, |state| {
                        self.skip(state)
                            .and_then(|state| self.parse_expr(&*expr, state))
                    })
                });
                times += 1;

                if current.is_err() {
                    if initial_result_is_err {
                        return Err(current.unwrap_err());
                    } else {
                        return Ok(current.unwrap_err());
                    }
                }

                result = self.skip(current.unwrap())
                    .and_then(|state| self.parse_expr(&*expr, state));
            }
        })
    }

    fn skip<'a, 'i>(
        &'a self,
        state: Box<ParserState<'i, &'a str>>
    ) -> ParseResult<Box<ParserState<'i, &'a str>>> {
        match (
            self.rules.contains_key("whitespace"),
            self.rules.contains_key("comment")
        ) {
            (false, false) => Ok(state),
            (true, false) => if state.atomicity() == Atomicity::NonAtomic {
                state.repeat(|state| self.parse_rule("whitespace", state))
            } else {
                Ok(state)
            },
            (false, true) => if state.atomicity() == Atomicity::NonAtomic {
                state.repeat(|state| self.parse_rule("comment", state))
            } else {
                Ok(state)
            },
            (true, true) => if state.atomicity() == Atomicity::NonAtomic {
                state.sequence(|state| {
                    state
                        .repeat(|state| self.parse_rule("whitespace", state))
                        .and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    self.parse_rule("comment", state).and_then(|state| {
                                        state.repeat(|state| self.parse_rule("whitespace", state))
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
