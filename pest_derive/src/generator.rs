// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;

use quote::{Ident, Tokens};

use super::ast::*;

pub fn generate(name: Ident, rules: Vec<Rule>, defaults: Vec<Ident>) -> Tokens {
    let mut predefined = HashMap::new();
    predefined.insert("any", quote! {
        fn any<I: ::pest::inputs::Input>(
            pos: ::pest::inputs::Position<I>,
            _: &mut ::pest::ParserState<Rule, I>
        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
            pos.skip(1)
        }
    });
    predefined.insert("eoi", quote! {
        fn eoi<I: ::pest::inputs::Input>(
            pos: ::pest::inputs::Position<I>,
            _: &mut ::pest::ParserState<Rule, I>
        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
            pos.at_end()
        }
    });
    predefined.insert("soi", quote! {
        fn soi<I: ::pest::inputs::Input>(
            pos: ::pest::inputs::Position<I>,
            _: &mut ::pest::ParserState<Rule, I>
        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
            pos.at_start()
        }
    });
    predefined.insert("peek", quote! {
        fn peek<I: ::pest::inputs::Input>(
            pos: ::pest::inputs::Position<I>,
            state: &mut ::pest::ParserState<Rule, I>
        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
            let string = state.stack.last().expect("peek was called on empty stack").as_str();
            pos.match_string(string)
        }
    });
    predefined.insert("pop", quote! {
        fn pop<I: ::pest::inputs::Input>(
            pos: ::pest::inputs::Position<I>,
            state: &mut ::pest::ParserState<Rule, I>
        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
            let pos = {
                let string = state.stack.last().expect("pop was called on empty stack").as_str();

                pos.match_string(string)
            };

            if pos.is_ok() {
                state.stack.pop().unwrap();
            }

            pos
        }
    });

    let rule_enum = generate_enum(&rules);
    let patterns = generate_patterns(&rules);
    let skip = generate_skip(&rules);

    let mut rules: Vec<_> = rules.into_iter().map(|rule| generate_rule(rule)).collect();
    rules.extend(defaults.into_iter().map(|name| predefined.get(name.as_ref()).unwrap().clone()));


    let parser_impl = quote! {
        impl ::pest::Parser<Rule> for #name {
            fn parse<I: ::pest::inputs::Input>(
                rule: Rule,
                input: ::std::rc::Rc<I>
            ) -> ::std::result::Result<::pest::iterators::Pairs<Rule, I>, ::pest::Error<Rule, I>> {
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
    let expr = if {
        rule.ty == RuleType::Atomic ||
        rule.ty == RuleType::CompoundAtomic ||
        &name == "whitespace" ||
        &name == "comment"
    } {
        generate_expr_atomic(rule.expr)
    } else {
        generate_expr(rule.expr)
    };

    match rule.ty {
        RuleType::Normal => quote! {
            #[allow(unused_variables)]
            pub fn #name<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                state.rule(Rule::#name, pos, |state, pos| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[allow(unused_variables)]
            pub fn #name<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[allow(unused_variables)]
            pub fn #name<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                state.rule(Rule::#name, pos, |state, pos| {
                    state.atomic(::pest::Atomicity::Atomic, move |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[allow(unused_variables)]
            pub fn #name<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, move |state| {
                    state.rule(Rule::#name, pos, |state, pos| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[allow(unused_variables)]
            pub fn #name<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                state.atomic(::pest::Atomicity::NonAtomic, move |state| {
                    state.rule(Rule::#name, pos, |state, pos| {
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
            #[allow(dead_code)]
            fn skip<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                _: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                Ok(pos)
            }
        },
        (true, false) => quote! {
            #[allow(dead_code)]
            fn skip<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    pos.repeat(|pos| {
                        whitespace(pos, state)
                    })
                } else {
                    Ok(pos)
                }
            }
        },
        (false, true) => quote! {
            #[allow(dead_code)]
            fn skip<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    pos.repeat(|pos| {
                        comment(pos, state)
                    })
                } else {
                    Ok(pos)
                }
            }
        },
        (true, true) => quote! {
            #[allow(dead_code)]
            fn skip<I: ::pest::inputs::Input>(
                pos: ::pest::inputs::Position<I>,
                state: &mut ::pest::ParserState<Rule, I>
            ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                if state.atomicity == ::pest::Atomicity::NonAtomic {
                    state.sequence(move |state| {
                        pos.sequence(|pos| {
                            pos.repeat(|pos| {
                                whitespace(pos, state)
                            }).and_then(|pos| {
                                pos.repeat(|pos| {
                                    state.sequence(move |state| {
                                        pos.sequence(|pos| {
                                            comment(pos, state).and_then(|pos| {
                                                pos.repeat(|pos| {
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
        },
        Expr::Ident(ident) => quote! {
            self::#ident(pos, state)
        },
        Expr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, move |state| {
                    pos.lookahead(true, |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, move |state| {
                    pos.lookahead(false, |pos| {
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
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        #head
                        #(
                            .and_then(|pos| {
                                self::skip(pos, state)
                            }).and_then(|pos| {
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
                    .or_else(|pos| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                pos.optional(|pos| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.optional(|pos| {
                            #expr
                        }).and_then(|pos| {
                            pos.repeat(|pos| {
                                state.sequence(move |state| {
                                    pos.sequence(|pos| {
                                        self::skip(pos, state).and_then(|pos| {
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
                            state.stack.push(start.span(end.clone()));
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
        },
        Expr::Ident(ident) => quote! {
            self::#ident(pos, state)
        },
        Expr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, move |state| {
                    pos.lookahead(true, |pos| {
                        #expr
                    })
                })
            }
        }
        Expr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, move |state| {
                    pos.lookahead(false, |pos| {
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
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        #head
                        #(
                            .and_then(|pos| {
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
                    .or_else(|pos| {
                        #tail
                    })
                )*
            }
        }
        Expr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                pos.optional(|pos| {
                    #expr
                })
            }
        }
        Expr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                pos.repeat(|pos| {
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
                            state.stack.push(start.span(end.clone()));
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
            }
        ];

        assert_eq!(generate_enum(&rules), quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                f
            }
        });
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

        assert_eq!(generate_expr(expr), quote! {
            state.sequence(move |state| {
                pos.sequence(|pos| {
                    pos.match_string("a").and_then(|pos| {
                        self::skip(pos, state)
                    }).and_then(|pos| {
                        pos.match_string("b")
                    }).and_then(|pos| {
                        self::skip(pos, state)
                    }).and_then(|pos| {
                        pos.match_string("c")
                    }).and_then(|pos| {
                        self::skip(pos, state)
                    }).and_then(|pos| {
                        pos.match_string("d")
                    })
                })
            })
        });
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

        assert_eq!(generate_expr_atomic(expr), quote! {
            state.sequence(move |state| {
                pos.sequence(|pos| {
                    pos.match_string("a").and_then(|pos| {
                        pos.match_string("b")
                    }).and_then(|pos| {
                        pos.match_string("c")
                    }).and_then(|pos| {
                        pos.match_string("d")
                    })
                })
            })
        });
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

        assert_eq!(generate_expr(expr), quote! {
            pos.match_string("a").or_else(|pos| {
                pos.match_string("b")
            }).or_else(|pos| {
                pos.match_string("c")
            }).or_else(|pos| {
                pos.match_string("d")
            })
        });
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

        assert_eq!(generate_expr_atomic(expr), quote! {
            pos.match_string("a").or_else(|pos| {
                pos.match_string("b")
            }).or_else(|pos| {
                pos.match_string("c")
            }).or_else(|pos| {
                pos.match_string("d")
            })
        });
    }

    #[test]
    fn expr_complex() {
        let expr = Expr::Choice(
            Box::new(Expr::Ident(Ident::new("a"))),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("'a'".to_owned(), "'b'".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::NegPred(
                        Box::new(Expr::Rep(
                            Box::new(Expr::Insens("b".to_owned()))
                        ))
                    )),
                    Box::new(Expr::PosPred(
                        Box::new(Expr::Opt(
                            Box::new(Expr::Rep(
                                Box::new(Expr::Choice(
                                    Box::new(Expr::Str("c".to_owned())),
                                    Box::new(Expr::Str("d".to_owned()))
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        );

        assert_eq!(generate_expr(expr), quote! {
            self::a(pos, state).or_else(|pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_range( 'a' .. 'b' ).and_then(|pos| {
                            self::skip(pos, state)
                        }).and_then(|pos| {
                            state.lookahead(false, move |state| {
                                pos.lookahead(false, |pos| {
                                    state.sequence(move |state| {
                                        pos.sequence(|pos| {
                                            pos.optional(|pos| {
                                                pos.match_insensitive("b")
                                            }).and_then(|pos| {
                                                pos.repeat(|pos| {
                                                    state.sequence(move |state| {
                                                        pos.sequence(|pos| {
                                                            self::skip(pos, state).and_then(|pos| {
                                                                pos.match_insensitive("b")
                                                            })
                                                        })
                                                    })
                                                })
                                            })
                                        })
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            self::skip(pos, state)
                        }).and_then(|pos| {
                            state.lookahead(true, move |state| {
                                pos.lookahead(true, |pos| {
                                    pos.optional(|pos| {
                                        state.sequence(move |state| {
                                            pos.sequence(|pos| {
                                                pos.optional(|pos| {
                                                    pos.match_string("c").or_else(|pos| {
                                                        pos.match_string("d")
                                                    })
                                                }).and_then(|pos| {
                                                    pos.repeat(|pos| {
                                                        state.sequence(move |state| {
                                                            pos.sequence(|pos| {
                                                                self::skip(pos, state)
                                                                     .and_then(|pos| {
                                                                         pos.match_string("c")
                                                                            .or_else(|pos| {
                                                                                pos.match_string(
                                                                                    "d"
                                                                                )
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
                            })
                        })
                    })
                })
            })
        });
    }

    #[test]
    fn expr_complex_atomic() {
        let expr = Expr::Choice(
            Box::new(Expr::Ident(Ident::new("a"))),
            Box::new(Expr::Seq(
                Box::new(Expr::Range("'a'".to_owned(), "'b'".to_owned())),
                Box::new(Expr::Seq(
                    Box::new(Expr::NegPred(
                        Box::new(Expr::Rep(
                            Box::new(Expr::Insens("b".to_owned()))
                        ))
                    )),
                    Box::new(Expr::PosPred(
                        Box::new(Expr::Opt(
                            Box::new(Expr::Rep(
                                Box::new(Expr::Choice(
                                    Box::new(Expr::Str("c".to_owned())),
                                    Box::new(Expr::Str("d".to_owned()))
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        );

        assert_eq!(generate_expr_atomic(expr), quote! {
            self::a(pos, state).or_else(|pos| {
                state.sequence(move |state| {
                    pos.sequence(|pos| {
                        pos.match_range('a'..'b').and_then(|pos| {
                            state.lookahead(false, move |state| {
                                pos.lookahead(false, |pos| {
                                    pos.repeat(|pos| {
                                        pos.match_insensitive("b")
                                    })
                                })
                            })
                        }).and_then(|pos| {
                            state.lookahead(true, move |state| {
                                pos.lookahead(true, |pos| {
                                    pos.optional(|pos| {
                                        pos.repeat(|pos| {
                                            pos.match_string("c").or_else(|pos| {
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
        });
    }

    #[test]
    fn generate_complete() {
        let name = Ident::new("MyParser");
        let rules = vec![Rule {
            name: Ident::new("a"),
            ty: RuleType::Silent,
            expr: Expr::Str("b".to_owned())
        }];
        let defaults = vec![Ident::new("any")];

        assert_eq!(generate(name, rules, defaults), quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                a
            }

            impl ::pest::Parser<Rule> for MyParser {
                fn parse<I: ::pest::inputs::Input>(
                    rule: Rule,
                    input: ::std::rc::Rc<I>
                ) -> ::std::result::Result<::pest::iterators::Pairs<Rule, I>, ::pest::Error<Rule, I>> {
                    mod rules {
                        use super::Rule;

                        #[allow(unused_variables)]
                        pub fn a<I: ::pest::inputs::Input>(
                            pos: ::pest::inputs::Position<I>,
                            state: &mut ::pest::ParserState<Rule, I>
                        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                            pos.match_string("b")
                        }

                        fn any<I: ::pest::inputs::Input>(
                            pos: ::pest::inputs::Position<I>,
                            _: &mut ::pest::ParserState<Rule, I>
                        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
                            pos.skip(1)
                        }

                        #[allow(dead_code)]
                        fn skip<I: ::pest::inputs::Input>(
                            pos: ::pest::inputs::Position<I>,
                            _: &mut ::pest::ParserState<Rule, I>
                        ) -> ::std::result::Result<::pest::inputs::Position<I>, ::pest::inputs::Position<I>> {
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
        });
    }
}
