// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;

use quote::{Ident, Tokens};

use ast::*;

pub fn generate(name: Ident, rules: Vec<Rule>, defaults: Vec<Ident>) -> Tokens {
    let mut predefined = HashMap::new();
    predefined.insert(
        "any",
        quote! {
            #[inline]
            fn any<'i>(
                pos: ::pest::Position<'i>,
                _: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                pos.skip(1)
            }
        }
    );
    predefined.insert(
        "eoi",
        quote! {
            #[inline]
            fn eoi<'i>(
                pos: ::pest::Position<'i>,
                _: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                pos.at_end()
            }
        }
    );
    predefined.insert(
        "soi",
        quote! {
            #[inline]
            fn soi<'i>(
                pos: ::pest::Position<'i>,
                _: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                pos.at_start()
            }
        }
    );
    predefined.insert(
        "peek",
        quote! {
            #[inline]
            fn peek<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                let string = state.stack.last().expect("peek was called on empty stack").as_str();
                pos.match_string(string)
            }
        }
    );
    predefined.insert(
        "pop",
        quote! {
            #[inline]
            fn pop<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                let pos = {
                    let string = state.stack.last()
                                      .expect("pop was called on empty stack").as_str();

                    pos.match_string(string)
                };

                if pos.is_ok() {
                    state.stack.pop().unwrap();
                }

                pos
            }
        }
    );

    let rule_enum = generate_enum(&rules);
    let patterns = generate_patterns(&rules);
    let skip = generate_skip(&rules);

    let mut rules: Vec<_> = rules.into_iter().map(|rule| generate_rule(rule)).collect();
    rules.extend(
        defaults
            .into_iter()
            .map(|name| predefined.get(name.as_ref()).unwrap().clone())
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

                ::pest::state(input, move |mut state, pos| {
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

fn generate_enum(rules: &Vec<Rule>) -> Tokens {
    let rules = rules.iter().map(|rule| &rule.name);

    quote! {
        #[allow(dead_code, non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Rule {
            #( #rules ),*
        }
    }
}

fn generate_patterns(rules: &Vec<Rule>) -> Tokens {
    let mut tokens = Tokens::new();

    let rules = rules.iter().map(|rule| {
        let rule = &rule.name;
        quote! {
            Rule::#rule => rules::#rule(pos, &mut state)
        }
    });

    tokens.append_separated(rules, ",");

    tokens
}

fn generate_rule(rule: Rule) -> Tokens {
    let name = rule.name;
    let expr = if { rule.ty == RuleType::Atomic || rule.ty == RuleType::CompoundAtomic } {
        generate_expr_atomic(rule.expr)
    } else {
        if &name == "whitespace" || &name == "comment" {
            let atomic = generate_expr_atomic(rule.expr);

            quote! {
                state.atomic(::pest::Atomicity::Atomic, #[inline(always)] move |state| {
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
            pub fn #name<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                state.rule(Rule::#name, pos, #[inline(always)] |state, pos| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                state.rule(Rule::#name, pos, #[inline(always)] |state, pos| {
                    state.atomic(::pest::Atomicity::Atomic, #[inline(always)] move |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, #[inline(always)] move |state| {
                    state.rule(Rule::#name, pos, #[inline(always)] |state, pos| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                state.atomic(::pest::Atomicity::NonAtomic, #[inline(always)] move |state| {
                    state.rule(Rule::#name, pos, #[inline(always)] |state, pos| {
                        #expr
                    })
                })
            }
        }
    }
}

fn generate_skip(rules: &Vec<Rule>) -> Tokens {
    let whitespace = rules.iter().any(|rule| rule.name.as_ref() == "whitespace");
    let comment = rules.iter().any(|rule| rule.name.as_ref() == "comment");

    match (whitespace, comment) {
        (false, false) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip<'i>(
                pos: ::pest::Position<'i>,
                _: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                Ok(pos)
            }
        },
        (true, false) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    pos.repeat(#[inline(always)] |pos| {
                        whitespace(pos, state)
                    })
                } else {
                    Ok(pos)
                }
            }
        },
        (false, true) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    pos.repeat(#[inline(always)] |pos| {
                        comment(pos, state)
                    })
                } else {
                    Ok(pos)
                }
            }
        },
        (true, true) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip<'i>(
                pos: ::pest::Position<'i>,
                state: &mut ::pest::ParserState<'i, Rule>
            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    state.sequence(#[inline(always)] move |state| {
                        pos.sequence(#[inline(always)] |pos| {
                            pos.repeat(#[inline(always)] |pos| {
                                whitespace(pos, state)
                            }).and_then(#[inline(always)] |pos| {
                                pos.repeat(#[inline(always)] |pos| {
                                    state.sequence(#[inline(always)] move |state| {
                                        pos.sequence(#[inline(always)] |pos| {
                                            comment(pos, state).and_then(|pos| {
                                                pos.repeat(#[inline(always)] |pos| {
                                                    whitespace(pos, state)
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
}

fn generate_expr(expr: Expr) -> Tokens {
    match expr {
        Expr::Str(string) => {
            let mut tokens = quote! {
                pos.match_string
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Insens(string) => {
            let mut tokens = quote! {
                pos.match_insensitive
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Range(start, end) => {
            let mut tokens = quote! {
                pos.match_range
            };

            tokens.append("(");
            tokens.append(start);
            tokens.append("..");
            tokens.append(end);
            tokens.append(")");

            tokens
        }
        Expr::Ident(ident) => quote! {
            self::#ident(pos, state)
        },
        Expr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, #[inline(always)] move |state| {
                    pos.lookahead(true, #[inline(always)] |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, #[inline(always)] move |state| {
                    pos.lookahead(false, #[inline(always)] |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::Seq(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            loop {
                match current {
                    Expr::Seq(lhs, rhs) => {
                        tail.push(generate_expr(*lhs));
                        current = *rhs;
                    }
                    expr => {
                        tail.push(generate_expr(expr));
                        break;
                    }
                }
            }

            quote! {
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        #head
                        #(
                            .and_then(#[inline(always)] |pos| {
                                self::skip(pos, state)
                            }).and_then(#[inline(always)] |pos| {
                                #tail
                            })
                        )*
                    })
                })
            }
        }
        Expr::Choice(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            loop {
                match current {
                    Expr::Choice(lhs, rhs) => {
                        tail.push(generate_expr(*lhs));
                        current = *rhs;
                    }
                    expr => {
                        tail.push(generate_expr(expr));
                        break;
                    }
                }
            }

            quote! {
                #head
                #(
                    .or_else(#[inline(always)] |pos| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                pos.optional(#[inline(always)] |pos| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        pos.optional(#[inline(always)] |pos| {
                            #expr
                        }).and_then(#[inline(always)] |pos| {
                            pos.repeat(#[inline(always)] |pos| {
                                state.sequence(#[inline(always)] move |state| {
                                    pos.sequence(#[inline(always)] |pos| {
                                        self::skip(pos, state).and_then(#[inline(always)] |pos| {
                                            #expr
                                        })
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
                    let start = pos.clone();

                    match #expr {
                        Ok(end) => {
                            state.stack.push(start.span(&end));
                            Ok(end)
                        }
                        Err(pos) => Err(pos)
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
                pos.match_string
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Insens(string) => {
            let mut tokens = quote! {
                pos.match_insensitive
            };

            tokens.append("(");
            tokens.append(format!("\"{}\"", string));
            tokens.append(")");

            tokens
        }
        Expr::Range(start, end) => {
            let mut tokens = quote! {
                pos.match_range
            };

            tokens.append("(");
            tokens.append(start);
            tokens.append("..");
            tokens.append(end);
            tokens.append(")");

            tokens
        }
        Expr::Ident(ident) => quote! {
            self::#ident(pos, state)
        },
        Expr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, #[inline(always)] move |state| {
                    pos.lookahead(true, #[inline(always)] |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, #[inline(always)] move |state| {
                    pos.lookahead(false, #[inline(always)] |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::Seq(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            loop {
                match current {
                    Expr::Seq(lhs, rhs) => {
                        tail.push(generate_expr_atomic(*lhs));
                        current = *rhs;
                    }
                    expr => {
                        tail.push(generate_expr_atomic(expr));
                        break;
                    }
                }
            }

            quote! {
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        #head
                        #(
                            .and_then(#[inline(always)] |pos| {
                                #tail
                            })
                        )*
                    })
                })
            }
        }
        Expr::Choice(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            loop {
                match current {
                    Expr::Choice(lhs, rhs) => {
                        tail.push(generate_expr_atomic(*lhs));
                        current = *rhs;
                    }
                    expr => {
                        tail.push(generate_expr_atomic(expr));
                        break;
                    }
                }
            }

            quote! {
                #head
                #(
                    .or_else(#[inline(always)] |pos| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                pos.optional(#[inline(always)] |pos| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                pos.repeat(#[inline(always)] |pos| {
                    #expr
                })
            }
        }
        Expr::Push(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                {
                    let start = pos.clone();

                    match #expr {
                        Ok(end) => {
                            state.stack.push(start.span(&end));
                            Ok(end)
                        }
                        Err(pos) => Err(pos)
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
                name: Ident::new("f"),
                ty: RuleType::Normal,
                expr: Expr::Ident(Ident::new("g"))
            },
        ];

        assert_eq!(
            generate_enum(&rules),
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
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        pos.match_string("a").and_then(#[inline(always)] |pos| {
                            self::skip(pos, state)
                        }).and_then(#[inline(always)] |pos| {
                            pos.match_string("b")
                        }).and_then(#[inline(always)] |pos| {
                            self::skip(pos, state)
                        }).and_then(#[inline(always)] |pos| {
                            pos.match_string("c")
                        }).and_then(#[inline(always)] |pos| {
                            self::skip(pos, state)
                        }).and_then(#[inline(always)] |pos| {
                            pos.match_string("d")
                        })
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
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        pos.match_string("a").and_then(#[inline(always)] |pos| {
                            pos.match_string("b")
                        }).and_then(#[inline(always)] |pos| {
                            pos.match_string("c")
                        }).and_then(#[inline(always)] |pos| {
                            pos.match_string("d")
                        })
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
                pos.match_string("a").or_else(#[inline(always)] |pos| {
                    pos.match_string("b")
                }).or_else(#[inline(always)] |pos| {
                    pos.match_string("c")
                }).or_else(#[inline(always)] |pos| {
                    pos.match_string("d")
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
                pos.match_string("a").or_else(#[inline(always)] |pos| {
                    pos.match_string("b")
                }).or_else(#[inline(always)] |pos| {
                    pos.match_string("c")
                }).or_else(#[inline(always)] |pos| {
                    pos.match_string("d")
                })
            }
        );
    }

    #[test]
    fn expr_complex() {
        let expr = Expr::Choice(
            Box::new(Expr::Ident(Ident::new("a"))),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("'a'".to_owned(), "'b'".to_owned())),
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
            state.sequence(#[inline(always)] move |state| {
                pos.sequence(#[inline(always)] |pos| {
                    self::skip(pos, state).and_then(
                        #[inline(always)] |pos| {
                            pos.match_insensitive("b")
                        }
                    )
                })
            })
        };
        let repeat = quote! {
            pos.repeat(#[inline(always)] |pos| {
                state.sequence(#[inline(always)] move |state| {
                    pos.sequence(#[inline(always)] |pos| {
                        self::skip(pos, state).and_then(#[inline(always)] |pos| {
                            pos.match_string("c").or_else(#[inline(always)] |pos| {
                                pos.match_string("d")
                            })
                         })
                    })
                })
            })
        };
        assert_eq!(
            generate_expr(expr),
            quote! {
                self::a(pos, state).or_else(#[inline(always)] |pos| {
                    state.sequence(#[inline(always)] move |state| {
                        pos.sequence(#[inline(always)] |pos| {
                            pos.match_range( 'a' .. 'b' ).and_then(#[inline(always)] |pos| {
                                self::skip(pos, state)
                            }).and_then(#[inline(always)] |pos| {
                                state.lookahead(false, #[inline(always)] move |state| {
                                    pos.lookahead(false, #[inline(always)] |pos| {
                                        state.sequence(#[inline(always)] move |state| {
                                            pos.sequence(#[inline(always)] |pos| {
                                                pos.optional(#[inline(always)] |pos| {
                                                    pos.match_insensitive("b")
                                                }).and_then(#[inline(always)] |pos| {
                                                    pos.repeat(#[inline(always)] |pos| {
                                                        #sequence
                                                    })
                                                })
                                            })
                                        })
                                    })
                                })
                            }).and_then(#[inline(always)] |pos| {
                                self::skip(pos, state)
                            }).and_then(#[inline(always)] |pos| {
                                state.lookahead(true, #[inline(always)] move |state| {
                                    pos.lookahead(true, #[inline(always)] |pos| {
                                        pos.optional(#[inline(always)] |pos| {
                                            state.sequence(#[inline(always)] move |state| {
                                                pos.sequence(#[inline(always)] |pos| {
                                                    pos.optional(#[inline(always)] |pos| {
                                                        pos.match_string("c").or_else(
                                                            #[inline(always)] |pos| {
                                                                pos.match_string("d")
                                                            }
                                                        )
                                                    }).and_then(#[inline(always)] |pos| {
                                                        #repeat
                                                    })
                                                })
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
            Box::new(Expr::Ident(Ident::new("a"))),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("'a'".to_owned(), "'b'".to_owned())),
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
                self::a(pos, state).or_else(#[inline(always)] |pos| {
                    state.sequence(#[inline(always)] move |state| {
                        pos.sequence(#[inline(always)] |pos| {
                            pos.match_range('a'..'b').and_then(#[inline(always)] |pos| {
                                state.lookahead(false, #[inline(always)] move |state| {
                                    pos.lookahead(false, #[inline(always)] |pos| {
                                        pos.repeat(#[inline(always)] |pos| {
                                            pos.match_insensitive("b")
                                        })
                                    })
                                })
                            }).and_then(#[inline(always)] |pos| {
                                state.lookahead(true, #[inline(always)] move |state| {
                                    pos.lookahead(true, #[inline(always)] |pos| {
                                        pos.optional(#[inline(always)] |pos| {
                                            pos.repeat(#[inline(always)] |pos| {
                                                pos.match_string("c")
                                                   .or_else(#[inline(always)] |pos| {
                                                    pos.match_string("d")
                                                })
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
    fn generate_complete() {
        let name = Ident::new("MyParser");
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Silent,
                expr: Expr::Str("b".to_owned())
            },
        ];
        let defaults = vec![Ident::new("any")];

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
                            pub fn a<'i>(
                                pos: ::pest::Position<'i>,
                                state: &mut ::pest::ParserState<'i, Rule>
                            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                                pos.match_string("b")
                            }

                            #[inline]
                            fn any<'i>(
                                pos: ::pest::Position<'i>,
                                _: &mut ::pest::ParserState<'i, Rule>
                            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                                pos.skip(1)
                            }

                            #[inline]
                            #[allow(dead_code)]
                            fn skip<'i>(
                                pos: ::pest::Position<'i>,
                                _: &mut ::pest::ParserState<'i, Rule>
                            ) -> ::std::result::Result<::pest::Position<'i>, ::pest::Position<'i>> {
                                Ok(pos)
                            }
                        }

                        ::pest::state(input, move |mut state, pos| {
                            match rule {
                                Rule::a => rules::a(pos, &mut state)
                            }
                        })
                    }
                }
            }
        );
    }
}
