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

use std::collections::HashMap;

use pest::{Atomicity, Error, ParserState, Position};
use pest::iterators::Pairs;

use pest_meta::ast::{Expr, Rule, RuleType};

pub struct Vm {
    rules: HashMap<String, Rule>
}

impl Vm {
    pub fn new(rules: Vec<Rule>) -> Vm {
        let rules = rules.into_iter().map(|r| (r.name.clone(), r)).collect();
        Vm { rules }
    }

    pub fn parse<'i>(
        &self,
        rule: &str,
        input: &'i str
    ) -> Option<Result<Pairs<'i, &str>, Error<'i, &str>>> {
        if let Some(rule) = self.rules.get(rule) {
            let result = pest::state(input, |mut state, pos| {
                self.parse_rule(rule, pos, &mut state)
            });

            Some(result)
        } else {
            None
        }
    }

    fn parse_rule<'a, 'i>(
        &self,
        rule: &'a Rule,
        pos: Position<'i>,
        state: &mut ParserState<'i, &'a str>
    ) -> Result<Position<'i>, Position<'i>> {
        if &rule.name == "whitespace" || &rule.name == "comment" {
            match rule.ty {
                RuleType::Normal => state.rule(&rule.name, pos, |state, pos| {
                    state.atomic(Atomicity::Atomic, move |state| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                }),
                RuleType::Silent => state.atomic(Atomicity::Atomic, move |state| {
                    self.parse_expr(&rule.expr, pos, state)
                }),
                RuleType::Atomic => state.rule(&rule.name, pos, |state, pos| {
                    state.atomic(Atomicity::Atomic, move |state| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                }),
                RuleType::CompoundAtomic => state.atomic(Atomicity::CompoundAtomic, move |state| {
                    state.rule(&rule.name, pos, |state, pos| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                }),
                RuleType::NonAtomic => state.atomic(Atomicity::Atomic, move |state| {
                    state.rule(&rule.name, pos, |state, pos| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                })
            }
        } else {
            match rule.ty {
                RuleType::Normal => state.rule(&rule.name, pos, |state, pos| {
                    self.parse_expr(&rule.expr, pos, state)
                }),
                RuleType::Silent => self.parse_expr(&rule.expr, pos, state),
                RuleType::Atomic => state.rule(&rule.name, pos, |state, pos| {
                    state.atomic(Atomicity::Atomic, move |state| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                }),
                RuleType::CompoundAtomic => state.atomic(Atomicity::CompoundAtomic, move |state| {
                    state.rule(&rule.name, pos, |state, pos| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                }),
                RuleType::NonAtomic => state.atomic(Atomicity::NonAtomic, move |state| {
                    state.rule(&rule.name, pos, |state, pos| {
                        self.parse_expr(&rule.expr, pos, state)
                    })
                })
            }
        }
    }

    fn parse_expr<'i>(
        &self,
        expr: &Expr,
        pos: Position<'i>,
        state: &mut ParserState<'i, &str>
    ) -> Result<Position<'i>, Position<'i>> {
        unimplemented!()
    }

    fn skip<'a, 'i>(
        &'a self,
        pos: Position<'i>,
        state: &mut ParserState<'i, &'a str>
    ) -> Result<Position<'i>, Position<'i>> {
        match (self.rules.get("whitespace"), self.rules.get("comment")) {
            (None, None) => Ok(pos),
            (Some(whitespace), None) => if state.atomicity == Atomicity::NonAtomic {
                pos.repeat(|pos| {
                    self.parse_rule(whitespace, pos, state)
                })
            } else {
                Ok(pos)
            },
            (None, Some(comment)) => if state.atomicity == Atomicity::NonAtomic {
                pos.repeat(|pos| {
                    self.parse_rule(comment, pos, state)
                })
            } else {
                Ok(pos)
            },
            (Some(whitespace), Some(comment)) => if state.atomicity == Atomicity::NonAtomic {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.repeat(|pos| {
                            self.parse_rule(whitespace, pos, state)
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        self.parse_rule(comment, pos, state).and_then(|pos| {
                                            pos.repeat(|pos| {
                                                self.parse_rule(whitespace, pos, state)
                                            })
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            } else {
                Ok(pos)
            }
        }
    }
}
