// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use quote::{Ident, Tokens};

use ast::*;

pub fn generate(name: Ident, rules: Vec<Rule>, defaults: Vec<&str>) -> Tokens {
    let mut predefined = HashMap::new();
    predefined.insert(
        "any",
        quote! {
            #[inline]
            #[allow(dead_code)]
            fn any(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.skip(1)
            }
        }
    );
    predefined.insert(
        "eoi",
        quote! {
            #[inline]
            pub fn eoi(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.rule(Rule::eoi, #[inline(always)] |state| {
                    state.end_of_input()
                })
            }
        }
    );
    predefined.insert(
        "soi",
        quote! {
            #[inline]
            fn soi(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.start_of_input()
            }
        }
    );
    predefined.insert(
        "peek",
        quote! {
            #[inline]
            fn peek(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                let string = state
                    .stack
                    .peek()
                    .expect("peek was called on empty stack")
                    .as_str();
                state.match_string(string)
            }
        }
    );
    predefined.insert(
        "pop",
        quote! {
            #[inline]
            fn pop(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
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
            }
        }
    );
    predefined.insert(
        "drop",
        quote! {
            #[inline]
            fn drop(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                match state.stack.pop() {
                    Some(_) => Ok(state),
                    None => Err(state),
                }
            }
        }
    );

    let uses_eoi = defaults.iter().any(|name| *name == "eoi");

    let rule_enum = generate_enum(&rules, uses_eoi);
    let patterns = generate_patterns(&rules, uses_eoi);
    let skip = generate_skip(&rules);

    let mut rules: Vec<_> = rules.into_iter().map(|rule| generate_rule(rule)).collect();
    rules.extend(
        defaults
            .into_iter()
            .map(|name| predefined.get(name).unwrap().clone())
    );

    let parser_impl = quote! {
        impl ::pest::Parser<Rule> for #name {
            fn parse<'i>(
                rule: Rule,
                input: &'i str
            ) -> ::std::result::Result<
                ::pest::iterators::Pairs<'i, Rule>,
                ::pest::Error<'i, Rule>
            > {
                mod rules {
                    use super::Rule;

                    #( #rules )*
                    #skip
                }

                ::pest::state(input, |state| {
                    match rule {
                        #patterns
                    }
                })
            }
        }
    };

    quote! {
        #rule_enum
        #parser_impl
    }
}

fn generate_enum(rules: &Vec<Rule>, uses_eoi: bool) -> Tokens {
    let rules = rules.iter().map(|rule| Ident::new(rule.name.as_str()));

    if uses_eoi {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                eoi,
                #( #rules ),*
            }
        }
    } else {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                #( #rules ),*
            }
        }
    }
}

fn generate_patterns(rules: &Vec<Rule>, uses_eoi: bool) -> Tokens {
    let mut tokens = Tokens::new();

    let mut rules: Vec<_> = rules
        .iter()
        .map(|rule| {
            let rule = Ident::new(rule.name.as_str());
            quote! {
                Rule::#rule => rules::#rule(state)
            }
        })
        .collect();

    if uses_eoi {
        rules.push(quote! {
            Rule::eoi => rules::eoi(state)
        });
    }

    tokens.append_separated(rules.iter(), ",");

    tokens
}

fn generate_rule(rule: Rule) -> Tokens {
    let name = Ident::new(rule.name);
    let expr = if { rule.ty == RuleType::Atomic || rule.ty == RuleType::CompoundAtomic } {
        generate_expr_atomic(rule.expr)
    } else {
        if name == "whitespace" || name == "comment" {
            let atomic = generate_expr_atomic(rule.expr);

            quote! {
                state.atomic(::pest::Atomicity::Atomic, #[inline(always)] |state| {
                    #atomic
                })
            }
        } else {
            generate_expr(rule.expr)
        }
    };

    match rule.ty {
        RuleType::Normal => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.rule(Rule::#name, #[inline(always)] |state| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.rule(Rule::#name, #[inline(always)] |state| {
                    state.atomic(::pest::Atomicity::Atomic, #[inline(always)] |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.atomic(::pest::Atomicity::CompoundAtomic, #[inline(always)] |state| {
                    state.rule(Rule::#name, #[inline(always)] |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                state.atomic(::pest::Atomicity::NonAtomic, #[inline(always)] |state| {
                    state.rule(Rule::#name, #[inline(always)] |state| {
                        #expr
                    })
                })
            }
        }
    }
}

fn generate_skip(rules: &Vec<Rule>) -> Tokens {
    let whitespace = rules.iter().any(|rule| rule.name == "whitespace");
    let comment = rules.iter().any(|rule| rule.name == "comment");

    match (whitespace, comment) {
        (false, false) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                Ok(state)
            }
        },
        (true, false) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    state.repeat(#[inline(always)] |state| {
                        whitespace(state)
                    })
                } else {
                    Ok(state)
                }
            }
        },
        (false, true) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    state.repeat(#[inline(always)] |state| {
                        comment(state)
                    })
                } else {
                    Ok(state)
                }
            }
        },
        (true, true) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    state.sequence(#[inline(always)] |state| {
                        state.repeat(#[inline(always)] |state| {
                            whitespace(state)
                        }).and_then(#[inline(always)] |state| {
                            state.repeat(#[inline(always)] |state| {
                                state.sequence(#[inline(always)] |state| {
                                    comment(state).and_then(#[inline(always)] |state| {
                                        state.repeat(#[inline(always)] |state| {
                                            whitespace(state)
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

fn generate_expr(expr: Expr) -> Tokens {
    match expr {
        Expr::Str(string) => {
            let mut tokens = quote! {
                state.match_string
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Insens(string) => {
            let mut tokens = quote! {
                state.match_insensitive
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Range(start, end) => {
            let mut tokens = quote! {
                state.match_range
            };

            tokens.append("(");
            tokens.append(format!("'{}'", start));
            tokens.append("..");
            tokens.append(format!("'{}'", end));
            tokens.append(")");

            tokens
        }
        Expr::Ident(ident) => {
            let ident = Ident::new(ident);
            quote! { self::#ident(state) }
        }
        Expr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, #[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, #[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::Seq(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let Expr::Seq(lhs, rhs) = current {
                tail.push(generate_expr(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr(current));

            quote! {
                state.sequence(#[inline(always)] |state| {
                    #head
                    #(
                        .and_then(#[inline(always)] |state| {
                            self::skip(state)
                        }).and_then(#[inline(always)] |state| {
                            #tail
                        })
                    )*
                })
            }
        }
        Expr::Choice(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let Expr::Choice(lhs, rhs) = current {
                tail.push(generate_expr(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr(current));

            quote! {
                #head
                #(
                    .or_else(#[inline(always)] |state| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.optional(#[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(#[inline(always)] |state| {
                    state.optional(#[inline(always)] |state| {
                        #expr.and_then(#[inline(always)] |state| {
                            state.repeat(#[inline(always)] |state| {
                                state.sequence(#[inline(always)] |state| {
                                    self::skip(
                                        state
                                    ).and_then(#[inline(always)] |state| {
                                        #expr
                                    })
                                })
                            })
                        })
                    })
                })
            }
        }
        Expr::Push(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                {
                    let start = state.clone_position();

                    match #expr {
                        Ok(mut state) => {
                            let end = state.clone_position();
                            state.stack.push(start.span(&end));
                            Ok(state)
                        }
                        Err(state) => Err(state)
                    }
                }
            }
        }
        _ => unreachable!()
    }
}

fn generate_expr_atomic(expr: Expr) -> Tokens {
    match expr {
        Expr::Str(string) => {
            let mut tokens = quote! {
                state.match_string
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Insens(string) => {
            let mut tokens = quote! {
                state.match_insensitive
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Range(start, end) => {
            let mut tokens = quote! {
                state.match_range
            };

            tokens.append("(");
            tokens.append(format!("'{}'", start));
            tokens.append("..");
            tokens.append(format!("'{}'", end));
            tokens.append(")");

            tokens
        }
        Expr::Ident(ident) => {
            let ident = Ident::new(ident);
            quote! { self::#ident(state) }
        }
        Expr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, #[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, #[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::Seq(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let Expr::Seq(lhs, rhs) = current {
                tail.push(generate_expr_atomic(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr_atomic(current));

            quote! {
                state.sequence(#[inline(always)] |state| {
                    #head
                    #(
                        .and_then(#[inline(always)] |state| {
                            #tail
                        })
                    )*
                })
            }
        }
        Expr::Choice(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let Expr::Choice(lhs, rhs) = current {
                tail.push(generate_expr_atomic(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr_atomic(current));

            quote! {
                #head
                #(
                    .or_else(#[inline(always)] |state| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.optional(#[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.repeat(#[inline(always)] |state| {
                    #expr
                })
            }
        }
        Expr::Skip(strings) => {
            let mut string_tokens = quote! {
                let strings =
            };

            string_tokens.append("[");

            for string in &strings[..strings.len() - 1] {
                string_tokens.append(format!("\"{}\"", string));
                string_tokens.append(",");
            }

            string_tokens.append(format!("\"{}\"", strings[strings.len() - 1]));

            string_tokens.append("]");
            string_tokens.append(";");

            quote! {
                #string_tokens

                let pos = state.clone_position();

                let new_pos = strings[1..].iter().fold(pos.clone().skip_until(strings[0]), |result, string| {
                    match (result, pos.clone().skip_until(string)) {
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
                });

                match new_pos {
                    Ok(pos) => {
                        Ok(state.with_updated_position(pos))
                    }
                    Err(pos) => {
                        Err(state.with_updated_position(pos))
                    }
                }
            }
        }
        Expr::Push(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                {
                    let start = state.clone_position();

                    match #expr {
                        Ok(mut state) => {
                            let end = state.clone_position();
                            state.stack.push(start.span(&end));
                            Ok(state)
                        }
                        Err(state) => Err(state)
                    }
                }
            }
        }
        _ => unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_simple() {
        let rules = vec![
            Rule {
                name: "f".to_owned(),
                ty: RuleType::Normal,
                expr: Expr::Ident("g".to_owned())
            },
        ];

        assert_eq!(
            generate_enum(&rules, false),
            quote! {
                #[allow(dead_code, non_camel_case_types)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    f
                }
            }
        );
    }

    #[test]
    fn sequence() {
        let expr = Expr::Seq(
            Box::new(Expr::Str("a".to_owned())),
            Box::new(Expr::Seq(
                Box::new(Expr::Str("b".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::Str("c".to_owned())),
                    Box::new(Expr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr(expr),
            quote! {
                state.sequence(#[inline(always)] |state| {
                    state.match_string("a").and_then(#[inline(always)] |state| {
                        self::skip(state)
                    }).and_then(#[inline(always)] |state| {
                        state.match_string("b")
                    }).and_then(#[inline(always)] |state| {
                        self::skip(state)
                    }).and_then(#[inline(always)] |state| {
                        state.match_string("c")
                    }).and_then(#[inline(always)] |state| {
                        self::skip(state)
                    }).and_then(#[inline(always)] |state| {
                        state.match_string("d")
                    })
                })
            }
        );
    }

    #[test]
    fn sequence_atomic() {
        let expr = Expr::Seq(
            Box::new(Expr::Str("a".to_owned())),
            Box::new(Expr::Seq(
                Box::new(Expr::Str("b".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::Str("c".to_owned())),
                    Box::new(Expr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                state.sequence(#[inline(always)] |state| {
                    state.match_string("a").and_then(#[inline(always)] |state| {
                        state.match_string("b")
                    }).and_then(#[inline(always)] |state| {
                        state.match_string("c")
                    }).and_then(#[inline(always)] |state| {
                        state.match_string("d")
                    })
                })
            }
        );
    }

    #[test]
    fn choice() {
        let expr = Expr::Choice(
            Box::new(Expr::Str("a".to_owned())),
            Box::new(Expr::Choice(
                Box::new(Expr::Str("b".to_owned())),
                Box::new(Expr::Choice(
                    Box::new(Expr::Str("c".to_owned())),
                    Box::new(Expr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr(expr),
            quote! {
                state.match_string("a").or_else(#[inline(always)] |state| {
                    state.match_string("b")
                }).or_else(#[inline(always)] |state| {
                    state.match_string("c")
                }).or_else(#[inline(always)] |state| {
                    state.match_string("d")
                })
            }
        );
    }

    #[test]
    fn choice_atomic() {
        let expr = Expr::Choice(
            Box::new(Expr::Str("a".to_owned())),
            Box::new(Expr::Choice(
                Box::new(Expr::Str("b".to_owned())),
                Box::new(Expr::Choice(
                    Box::new(Expr::Str("c".to_owned())),
                    Box::new(Expr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                state.match_string("a").or_else(#[inline(always)] |state| {
                    state.match_string("b")
                }).or_else(#[inline(always)] |state| {
                    state.match_string("c")
                }).or_else(#[inline(always)] |state| {
                    state.match_string("d")
                })
            }
        );
    }

    #[test]
    fn skip() {
        let expr = Expr::Skip(vec!["a".to_owned(), "b".to_owned()]);

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                let strings = ["a", "b"];

                let pos = state.clone_position();

                let new_pos = strings[1..].iter().fold(pos.clone().skip_until(strings[0]), |result, string| {
                    match (result, pos.clone().skip_until(string)) {
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
                });

                match new_pos {
                    Ok(pos) => {
                        Ok(state.with_updated_position(pos))
                    }
                    Err(pos) => {
                        Err(state.with_updated_position(pos))
                    }
                }
            }
        );
    }

    #[test]
    fn expr_complex() {
        let expr = Expr::Choice(
            Box::new(Expr::Ident("a".to_owned())),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("a".to_owned(), "b".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::NegPred(Box::new(Expr::Rep(Box::new(Expr::Insens(
                        "b".to_owned()
                    )))))),
                    Box::new(Expr::PosPred(Box::new(Expr::Opt(Box::new(Expr::Rep(
                        Box::new(Expr::Choice(
                            Box::new(Expr::Str("c".to_owned())),
                            Box::new(Expr::Str("d".to_owned()))
                        ))
                    ))))))
                ))
            ))
        );

        let sequence = quote! {
            state.sequence(#[inline(always)] |state| {
                self::skip(state).and_then(
                    #[inline(always)] |state| {
                        state.match_insensitive("b")
                    }
                )
            })
        };
        let repeat = quote! {
            state.repeat(#[inline(always)] |state| {
                state.sequence(#[inline(always)] |state| {
                    self::skip(state).and_then(#[inline(always)] |state| {
                        state.match_string("c").or_else(#[inline(always)] |state| {
                            state.match_string("d")
                        })
                     })
                })
            })
        };
        assert_eq!(
            generate_expr(expr),
            quote! {
                self::a(state).or_else(#[inline(always)] |state| {
                    state.sequence(#[inline(always)] |state| {
                        state.match_range('a'..'b').and_then(#[inline(always)] |state| {
                            self::skip(state)
                        }).and_then(#[inline(always)] |state| {
                            state.lookahead(false, #[inline(always)] |state| {
                                state.sequence(#[inline(always)] |state| {
                                    state.optional(#[inline(always)] |state| {
                                        state.match_insensitive(
                                            "b"
                                        ).and_then(#[inline(always)] |state| {
                                            state.repeat(#[inline(always)] |state| {
                                                #sequence
                                            })
                                        })
                                    })
                                })
                            })
                        }).and_then(#[inline(always)] |state| {
                            self::skip(state)
                        }).and_then(#[inline(always)] |state| {
                            state.lookahead(true, #[inline(always)] |state| {
                                state.optional(#[inline(always)] |state| {
                                    state.sequence(#[inline(always)] |state| {
                                        state.optional(#[inline(always)] |state| {
                                            state.match_string("c").or_else(
                                                #[inline(always)] |state| {
                                                    state.match_string("d")
                                                }
                                            ).and_then(#[inline(always)] |state| {
                                                #repeat
                                            })
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            }
        );
    }

    #[test]
    fn expr_complex_atomic() {
        let expr = Expr::Choice(
            Box::new(Expr::Ident("a".to_owned())),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("a".to_owned(), "b".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::NegPred(Box::new(Expr::Rep(Box::new(Expr::Insens(
                        "b".to_owned()
                    )))))),
                    Box::new(Expr::PosPred(Box::new(Expr::Opt(Box::new(Expr::Rep(
                        Box::new(Expr::Choice(
                            Box::new(Expr::Str("c".to_owned())),
                            Box::new(Expr::Str("d".to_owned()))
                        ))
                    ))))))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                self::a(state).or_else(#[inline(always)] |state| {
                    state.sequence(#[inline(always)] |state| {
                        state.match_range('a'..'b').and_then(#[inline(always)] |state| {
                            state.lookahead(false, #[inline(always)] |state| {
                                state.repeat(#[inline(always)] |state| {
                                    state.match_insensitive("b")
                                })
                            })
                        }).and_then(#[inline(always)] |state| {
                            state.lookahead(true, #[inline(always)] |state| {
                                state.optional(#[inline(always)] |state| {
                                    state.repeat(#[inline(always)] |state| {
                                        state.match_string("c")
                                           .or_else(#[inline(always)] |state| {
                                            state.match_string("d")
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            }
        );
    }

    #[test]
    fn generate_complete() {
        let name = Ident::new("MyParser");
        let rules = vec![
            Rule {
                name: "a".to_owned(),
                ty: RuleType::Silent,
                expr: Expr::Str("b".to_owned())
            },
        ];
        let defaults = vec!["any"];

        assert_eq!(
            generate(name, rules, defaults),
            quote! {
                #[allow(dead_code, non_camel_case_types)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    a
                }

            impl ::pest::Parser<Rule> for MyParser {
                fn parse<'i>(
                    rule: Rule,
                    input: &'i str
                ) -> ::std::result::Result<
                    ::pest::iterators::Pairs<'i, Rule>,
                    ::pest::Error<'i, Rule>
                > {
                    mod rules {
                        use super::Rule;

                            #[inline]
                            #[allow(unused_variables)]
                            pub fn a(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                                state.match_string("b")
                            }

                            #[inline]
                            #[allow(dead_code)]
                            fn any(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                                state.skip(1)
                            }

                            #[inline]
                            #[allow(dead_code)]
                            fn skip(state: ::pest::ParserState<Rule>) -> ::pest::ParseResult<Rule> {
                                Ok(state)
                            }
                        }

                        ::pest::state(input, |state| {
                            match rule {
                                Rule::a => rules::a(state)
                            }
                        })
                    }
                }
            }
        );
    }
}
