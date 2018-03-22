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
use std::char;
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
            "peek" => {
                return {
                    let string = state
                        .stack
                        .peek()
                        .expect("peek was called on empty stack")
                        .as_str();
                    state.match_string(string)
                };
            }
            "pop" => {
                return {
                    let result_state = {
                        let string = state
                            .stack
                            .peek()
                            .expect("pop was called on empty stack")
                            .as_str();

                        state.match_string(string)
                    };

                    match result_state {
                        Ok(mut state) => {
                            state.stack.pop();
                            Ok(state)
                        }
                        Err(state) => Err(state)
                    }
                };
            }
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
            Expr::Str(ref string) => {
                state.match_string(&unescape(string).expect("incorrect string literal"))
            }
            Expr::Insens(ref string) => {
                state.match_insensitive(&unescape(string).expect("incorrect string literal"))
            }
            Expr::Range(ref start, ref end) => {
                let start = unescape(start)
                    .expect("incorrect char literal")
                    .chars()
                    .next()
                    .expect("empty char literal");
                let end = unescape(end)
                    .expect("incorrect char literal")
                    .chars()
                    .next()
                    .expect("empty char literal");

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
            Expr::Push(ref expr) => {
                let start = state.clone_position();

                match self.parse_expr(&*expr, state) {
                    Ok(mut state) => {
                        let end = state.clone_position();
                        state.stack.push(start.span(&end));
                        Ok(state)
                    }
                    Err(state) => Err(state)
                }
            }
            Expr::Skip(ref strings) => {
                let new_pos = strings[1..].iter().fold(
                    {
                        let mut pos = state.clone_position();
                        if pos.skip_until(&strings[0]) {
                            Ok(pos)
                        } else {
                            Err(pos)
                        }
                    },
                    |result, string| {
                        let mut pos = state.clone_position();
                        let new_result = if pos.skip_until(string) {
                            Ok(pos)
                        } else {
                            Err(pos)
                        };
                        match (result, new_result) {
                            (Ok(lhs), Ok(rhs)) => {
                                if rhs.pos() < lhs.pos() {
                                    Ok(rhs)
                                } else {
                                    Ok(lhs)
                                }
                            }
                            (Ok(lhs), Err(_)) => Ok(lhs),
                            (Err(_), Ok(rhs)) => Ok(rhs),
                            (Err(lhs), Err(_)) => Err(lhs)
                        }
                    }
                );

                match new_pos {
                    Ok(pos) => Ok(state.with_updated_position(pos)),
                    Err(pos) => Err(state.with_updated_position(pos))
                }
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

                let original_pos = match result {
                    Ok(ref state) => Ok(state.clone_position()),
                    Err(ref state) => Err(state.clone_position())
                };

                let current = result.and_then(|state| {
                    self.skip(state)
                        .and_then(|state| self.parse_expr(&*expr, state))
                });
                times += 1;

                if current.is_err() {
                    return match original_pos {
                        Ok(pos) => Ok(current.unwrap_err().with_updated_position(pos)),
                        Err(pos) => Err(current.unwrap_err().with_updated_position(pos))
                    };
                }

                result = current;
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
            (true, false) => if state.atomicity == Atomicity::NonAtomic {
                state.repeat(|state| self.parse_rule("whitespace", state))
            } else {
                Ok(state)
            },
            (false, true) => if state.atomicity == Atomicity::NonAtomic {
                state.repeat(|state| self.parse_rule("comment", state))
            } else {
                Ok(state)
            },
            (true, true) => if state.atomicity == Atomicity::NonAtomic {
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

fn unescape(string: &str) -> Option<String> {
    let mut result = String::new();
    let mut chars = string.chars();

    loop {
        match chars.next() {
            Some('\\') => match chars.next()? {
                '"' => result.push('"'),
                '\\' => result.push('\\'),
                'r' => result.push('\r'),
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '0' => result.push('\0'),
                '\'' => result.push('\''),
                'x' => {
                    let string: String = chars.clone().take(2).collect();

                    if string.len() != 2 {
                        return None;
                    }

                    for _ in 0..string.len() {
                        chars.next()?;
                    }

                    let value = u8::from_str_radix(&string, 16).ok()?;

                    result.push(char::from(value));
                }
                'u' => {
                    if chars.next()? != '{' {
                        return None;
                    }

                    let string: String = chars.clone().take_while(|c| *c != '}').collect();

                    if string.len() < 2 || 6 < string.len() {
                        return None;
                    }

                    for _ in 0..string.len() + 1 {
                        chars.next()?;
                    }

                    let value = u32::from_str_radix(&string, 16).ok()?;

                    result.push(char::from_u32(value)?);
                }
                _ => return None
            },
            Some(c) => result.push(c),
            None => return Some(result)
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn unescape_all() {
        let string = r"a\nb\x55c\u{111}d";

        assert_eq!(unescape(string), Some("a\nb\x55c\u{111}d".to_owned()));
    }

    #[test]
    fn unescape_empty_escape() {
        let string = r"\";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_wrong_escape() {
        let string = r"\w";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_wrong_byte() {
        let string = r"\xfg";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_short_byte() {
        let string = r"\xf";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_no_open_brace_unicode() {
        let string = r"\u11";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_no_close_brace_unicode() {
        let string = r"\u{11";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_short_unicode() {
        let string = r"\u{1}";

        assert_eq!(unescape(string), None);
    }

    #[test]
    fn unescape_long_unicode() {
        let string = r"\u{1111111}";

        assert_eq!(unescape(string), None);
    }
}
