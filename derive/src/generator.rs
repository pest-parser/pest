// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use quote::Tokens;
use syn::Ident;

use ast::*;

pub fn generate(name: Ident, rules: Vec<Rule>, defaults: Vec<&str>) -> Tokens {
    let mut predefined = HashMap::new();
    predefined.insert(
        "any",
        quote! {
            #[inline]
            #[allow(dead_code)]
            fn any(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.skip(1)
            }
        }
    );
    predefined.insert(
        "eoi",
        quote! {
            #[inline]
            pub fn eoi(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.rule(Rule::eoi, |state| {
                    state.end_of_input()
                })
            }
        }
    );
    predefined.insert(
        "soi",
        quote! {
            #[inline]
            fn soi(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.start_of_input()
            }
        }
    );
    predefined.insert(
        "peek",
        quote! {
            #[inline]
            fn peek(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.stack_peek()
            }
        }
    );
    predefined.insert(
        "pop",
        quote! {
            #[inline]
            fn pop(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.stack_pop()
            }
        }
    );
    predefined.insert(
        "drop",
        quote! {
            #[inline]
            fn drop(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.stack_drop()
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
    let rules = rules.iter().map(|rule| Ident::from(rule.name.as_str()));
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
    let mut rules: Vec<Tokens> = rules
        .iter()
        .map(|rule| {
            let rule = Ident::from(rule.name.as_str());
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

    quote! {
        #( #rules ),*
    }
}

fn generate_rule(rule: Rule) -> Tokens {
    let name = Ident::from(rule.name);
    let expr = if { rule.ty == RuleType::Atomic || rule.ty == RuleType::CompoundAtomic } {
        generate_expr_atomic(rule.expr)
    } else {
        if name == "whitespace" || name == "comment" {
            let atomic = generate_expr_atomic(rule.expr);

            quote! {
                state.atomic(::pest::Atomicity::Atomic, |state| {
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
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.rule(Rule::#name, |state| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.rule(Rule::#name, |state| {
                    state.atomic(::pest::Atomicity::Atomic, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, |state| {
                    state.rule(Rule::#name, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.atomic(::pest::Atomicity::NonAtomic, |state| {
                    state.rule(Rule::#name, |state| {
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
            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                Ok(state)
            }
        },
        (true, false) => quote! {
            #[inline]
            #[allow(dead_code)]
            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                if state.atomicity() == ::pest::Atomicity::NonAtomic {
                    state.repeat(|state| {
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
            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                if state.atomicity() == ::pest::Atomicity::NonAtomic {
                    state.repeat(|state| {
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
            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                if state.atomicity() == ::pest::Atomicity::NonAtomic {
                    state.sequence(|state| {
                        state.repeat(|state| {
                            whitespace(state)
                        }).and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    comment(state).and_then(|state| {
                                        state.repeat(|state| {
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
            quote! {
                state.match_string(#string)
            }
        }
        Expr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        Expr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        Expr::Ident(ident) => {
            let ident = Ident::from(ident);
            quote! { self::#ident(state) }
        }
        Expr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, |state| {
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
                state.sequence(|state| {
                    #head
                    #(
                        .and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
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
                    .or_else(|state| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(|state| {
                    state.optional(|state| {
                        #expr.and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    self::skip(
                                        state
                                    ).and_then(|state| {
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
                    state.stack_push(|state| {
                        #expr
                    })
                }
            }
        }
        _ => unreachable!()
    }
}

fn generate_expr_atomic(expr: Expr) -> Tokens {
    match expr {
        Expr::Str(string) => {
            quote! {
                state.match_string(#string)
            }
        }
        Expr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        Expr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        Expr::Ident(ident) => {
            let ident = Ident::from(ident);
            quote! { self::#ident(state) }
        }
        Expr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, |state| {
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
                state.sequence(|state| {
                    #head
                    #(
                        .and_then(|state| {
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
                    .or_else(|state| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.repeat(|state| {
                    #expr
                })
            }
        }
        Expr::Skip(strings) => {
            quote! {
                let strings = [#(#strings),*];

                state.skip_until_any(&strings)
            }
        }
        Expr::Push(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                {
                    state.stack_push(|state| {
                        #expr
                    })
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
                state.sequence(|state| {
                    state.match_string("a").and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
                        state.match_string("b")
                    }).and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
                        state.match_string("c")
                    }).and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
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
                state.sequence(|state| {
                    state.match_string("a").and_then(|state| {
                        state.match_string("b")
                    }).and_then(|state| {
                        state.match_string("c")
                    }).and_then(|state| {
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
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
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
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
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

                state.skip_until_any(&strings)
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
            state.sequence(|state| {
                self::skip(state).and_then(
                    |state| {
                        state.match_insensitive("b")
                    }
                )
            })
        };
        let repeat = quote! {
            state.repeat(|state| {
                state.sequence(|state| {
                    self::skip(state).and_then(|state| {
                        state.match_string("c").or_else(|state| {
                            state.match_string("d")
                        })
                     })
                })
            })
        };
        assert_eq!(
            generate_expr(expr),
            quote! {
                self::a(state).or_else(|state| {
                    state.sequence(|state| {
                        state.match_range('a'..'b').and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
                            state.lookahead(false, |state| {
                                state.sequence(|state| {
                                    state.optional(|state| {
                                        state.match_insensitive(
                                            "b"
                                        ).and_then(|state| {
                                            state.repeat(|state| {
                                                #sequence
                                            })
                                        })
                                    })
                                })
                            })
                        }).and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
                            state.lookahead(true, |state| {
                                state.optional(|state| {
                                    state.sequence(|state| {
                                        state.optional(|state| {
                                            state.match_string("c").or_else(
                                                |state| {
                                                    state.match_string("d")
                                                }
                                            ).and_then(|state| {
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
                self::a(state).or_else(|state| {
                    state.sequence(|state| {
                        state.match_range('a'..'b').and_then(|state| {
                            state.lookahead(false, |state| {
                                state.repeat(|state| {
                                    state.match_insensitive("b")
                                })
                            })
                        }).and_then(|state| {
                            state.lookahead(true, |state| {
                                state.optional(|state| {
                                    state.repeat(|state| {
                                        state.match_string("c")
                                           .or_else(|state| {
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
        let name = Ident::from("MyParser");
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
                            pub fn a(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                                state.match_string("b")
                            }

                            #[inline]
                            #[allow(dead_code)]
                            fn any(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                                state.skip(1)
                            }

                            #[inline]
                            #[allow(dead_code)]
                            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
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
