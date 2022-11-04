// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{self, Generics, Ident};

use pest_meta::ast::*;
use pest_meta::optimizer::*;
use pest_meta::UNICODE_PROPERTY_NAMES;

pub fn generate(
    name: Ident,
    generics: &Generics,
    path: Option<PathBuf>,
    rules: Vec<OptimizedRule>,
    defaults: Vec<&str>,
    include_grammar: bool,
) -> TokenStream {
    let uses_eoi = defaults.iter().any(|name| *name == "EOI");

    let builtins = generate_builtin_rules();
    let include_fix = if include_grammar {
        match path {
            Some(ref path) => generate_include(&name, path.to_str().expect("non-Unicode path")),
            None => quote!(),
        }
    } else {
        quote!()
    };
    let rule_enum = generate_enum(&rules, uses_eoi);
    let patterns = generate_patterns(&rules, uses_eoi);
    let skip = generate_skip(&rules);

    let mut rules: Vec<_> = rules.into_iter().map(generate_rule).collect();
    rules.extend(builtins.into_iter().filter_map(|(builtin, tokens)| {
        if defaults.contains(&builtin) {
            Some(tokens)
        } else {
            None
        }
    }));

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let result = result_type();

    let parser_impl = quote! {
        #[allow(clippy::all)]
        impl #impl_generics ::pest::Parser<Rule> for #name #ty_generics #where_clause {
            fn parse<'i>(
                rule: Rule,
                input: &'i str
            ) -> #result<
                ::pest::iterators::Pairs<'i, Rule>,
                ::pest::error::Error<Rule>
            > {
                mod rules {
                    #![allow(clippy::upper_case_acronyms)]
                    pub mod hidden {
                        use super::super::Rule;
                        #skip
                    }

                    pub mod visible {
                        use super::super::Rule;
                        #( #rules )*
                    }

                    pub use self::visible::*;
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
        #include_fix
        #rule_enum
        #parser_impl
    }
}

// Note: All builtin rules should be validated as pest builtins in meta/src/validator.rs.
// Some should also be keywords.
fn generate_builtin_rules() -> Vec<(&'static str, TokenStream)> {
    let mut builtins = Vec::new();

    insert_builtin!(builtins, ANY, state.skip(1));
    insert_builtin!(
        builtins,
        EOI,
        state.rule(Rule::EOI, |state| state.end_of_input())
    );
    insert_builtin!(builtins, SOI, state.start_of_input());
    insert_builtin!(builtins, PEEK, state.stack_peek());
    insert_builtin!(builtins, PEEK_ALL, state.stack_match_peek());
    insert_builtin!(builtins, POP, state.stack_pop());
    insert_builtin!(builtins, POP_ALL, state.stack_match_pop());
    insert_builtin!(builtins, DROP, state.stack_drop());

    insert_builtin!(builtins, ASCII_DIGIT, state.match_range('0'..'9'));
    insert_builtin!(builtins, ASCII_NONZERO_DIGIT, state.match_range('1'..'9'));
    insert_builtin!(builtins, ASCII_BIN_DIGIT, state.match_range('0'..'1'));
    insert_builtin!(builtins, ASCII_OCT_DIGIT, state.match_range('0'..'7'));
    insert_builtin!(
        builtins,
        ASCII_HEX_DIGIT,
        state
            .match_range('0'..'9')
            .or_else(|state| state.match_range('a'..'f'))
            .or_else(|state| state.match_range('A'..'F'))
    );
    insert_builtin!(builtins, ASCII_ALPHA_LOWER, state.match_range('a'..'z'));
    insert_builtin!(builtins, ASCII_ALPHA_UPPER, state.match_range('A'..'Z'));
    insert_builtin!(
        builtins,
        ASCII_ALPHA,
        state
            .match_range('a'..'z')
            .or_else(|state| state.match_range('A'..'Z'))
    );
    insert_builtin!(
        builtins,
        ASCII_ALPHANUMERIC,
        state
            .match_range('a'..'z')
            .or_else(|state| state.match_range('A'..'Z'))
            .or_else(|state| state.match_range('0'..'9'))
    );
    insert_builtin!(builtins, ASCII, state.match_range('\x00'..'\x7f'));
    insert_builtin!(
        builtins,
        NEWLINE,
        state
            .match_string("\n")
            .or_else(|state| state.match_string("\r\n"))
            .or_else(|state| state.match_string("\r"))
    );

    let box_ty = box_type();

    for property in UNICODE_PROPERTY_NAMES {
        let property_ident: Ident = syn::parse_str(property).unwrap();
        // insert manually for #property substitution
        builtins.push((property, quote! {
            #[inline]
            #[allow(dead_code, non_snake_case, unused_variables)]
            fn #property_ident(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                state.match_char_by(::pest::unicode::#property_ident)
            }
        }));
    }
    builtins
}

// Needed because Cargo doesn't watch for changes in grammars.
fn generate_include(name: &Ident, path: &str) -> TokenStream {
    let const_name = Ident::new(&format!("_PEST_GRAMMAR_{}", name), Span::call_site());
    // Need to make this relative to the current directory since the path to the file
    // is derived from the CARGO_MANIFEST_DIR environment variable
    let mut current_dir = std::env::current_dir().expect("Unable to get current directory");
    current_dir.push(path);
    let relative_path = current_dir.to_str().expect("path contains invalid unicode");
    quote! {
        #[allow(non_upper_case_globals)]
        const #const_name: &'static str = include_str!(#relative_path);
    }
}

fn generate_enum(rules: &[OptimizedRule], uses_eoi: bool) -> TokenStream {
    let rules = rules
        .iter()
        .map(|rule| Ident::new(rule.name.as_str(), Span::call_site()));
    if uses_eoi {
        quote! {
            #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                EOI,
                #( #rules ),*
            }
        }
    } else {
        quote! {
            #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                #( #rules ),*
            }
        }
    }
}

fn generate_patterns(rules: &[OptimizedRule], uses_eoi: bool) -> TokenStream {
    let mut rules: Vec<TokenStream> = rules
        .iter()
        .map(|rule| {
            let rule = Ident::new(rule.name.as_str(), Span::call_site());
            quote! {
                Rule::#rule => rules::#rule(state)
            }
        })
        .collect();

    if uses_eoi {
        rules.push(quote! {
            Rule::EOI => rules::EOI(state)
        });
    }

    quote! {
        #( #rules ),*
    }
}

fn generate_rule(rule: OptimizedRule) -> TokenStream {
    let name = Ident::new(&rule.name, Span::call_site());
    let expr = if rule.ty == RuleType::Atomic || rule.ty == RuleType::CompoundAtomic {
        generate_expr_atomic(rule.expr)
    } else if name == "WHITESPACE" || name == "COMMENT" {
        let atomic = generate_expr_atomic(rule.expr);

        quote! {
            state.atomic(::pest::Atomicity::Atomic, |state| {
                #atomic
            })
        }
    } else {
        generate_expr(rule.expr)
    };

    let box_ty = box_type();

    match rule.ty {
        RuleType::Normal => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                state.rule(Rule::#name, |state| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                state.rule(Rule::#name, |state| {
                    state.atomic(::pest::Atomicity::Atomic, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, |state| {
                    state.rule(Rule::#name, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                state.atomic(::pest::Atomicity::NonAtomic, |state| {
                    state.rule(Rule::#name, |state| {
                        #expr
                    })
                })
            }
        },
    }
}

fn generate_skip(rules: &[OptimizedRule]) -> TokenStream {
    let whitespace = rules.iter().any(|rule| rule.name == "WHITESPACE");
    let comment = rules.iter().any(|rule| rule.name == "COMMENT");

    match (whitespace, comment) {
        (false, false) => generate_rule!(skip, Ok(state)),
        (true, false) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.repeat(|state| super::visible::WHITESPACE(state))
            } else {
                Ok(state)
            }
        ),
        (false, true) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.repeat(|state| super::visible::COMMENT(state))
            } else {
                Ok(state)
            }
        ),
        (true, true) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.sequence(|state| {
                    state
                        .repeat(|state| super::visible::WHITESPACE(state))
                        .and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    super::visible::COMMENT(state).and_then(|state| {
                                        state.repeat(|state| super::visible::WHITESPACE(state))
                                    })
                                })
                            })
                        })
                })
            } else {
                Ok(state)
            }
        ),
    }
}

fn generate_expr(expr: OptimizedExpr) -> TokenStream {
    match expr {
        OptimizedExpr::Str(string) => {
            quote! {
                state.match_string(#string)
            }
        }
        OptimizedExpr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        OptimizedExpr::Ident(ident) => {
            let ident = Ident::new(&ident, Span::call_site());
            quote! { self::#ident(state) }
        }
        OptimizedExpr::PeekSlice(start, end_) => {
            let end = QuoteOption(end_);
            quote! {
                state.stack_match_peek_slice(#start, #end, ::pest::MatchDir::BottomToTop)
            }
        }
        OptimizedExpr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Seq(lhs, rhs) = current {
                tail.push(generate_expr(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr(current));

            quote! {
                state.sequence(|state| {
                    #head
                    #(
                        .and_then(|state| {
                            super::hidden::skip(state)
                        }).and_then(|state| {
                            #tail
                        })
                    )*
                })
            }
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Choice(lhs, rhs) = current {
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
        OptimizedExpr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(|state| {
                    state.optional(|state| {
                        #expr.and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    super::hidden::skip(
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
        OptimizedExpr::Skip(strings) => {
            quote! {
                let strings = [#(#strings),*];

                state.skip_until(&strings)
            }
        }
        OptimizedExpr::Push(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.stack_push(|state| #expr)
            }
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.restore_on_err(|state| #expr)
            }
        }
    }
}

fn generate_expr_atomic(expr: OptimizedExpr) -> TokenStream {
    match expr {
        OptimizedExpr::Str(string) => {
            quote! {
                state.match_string(#string)
            }
        }
        OptimizedExpr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        OptimizedExpr::Ident(ident) => {
            let ident = Ident::new(&ident, Span::call_site());
            quote! { self::#ident(state) }
        }
        OptimizedExpr::PeekSlice(start, end_) => {
            let end = QuoteOption(end_);
            quote! {
                state.stack_match_peek_slice(#start, #end, ::pest::MatchDir::BottomToTop)
            }
        }
        OptimizedExpr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Seq(lhs, rhs) = current {
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
        OptimizedExpr::Choice(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Choice(lhs, rhs) = current {
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
        OptimizedExpr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.repeat(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Skip(strings) => {
            quote! {
                let strings = [#(#strings),*];

                state.skip_until(&strings)
            }
        }
        OptimizedExpr::Push(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.stack_push(|state| #expr)
            }
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.restore_on_err(|state| #expr)
            }
        }
    }
}

struct QuoteOption<T>(Option<T>);

impl<T: ToTokens> ToTokens for QuoteOption<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let option = option_type();
        tokens.append_all(match self.0 {
            Some(ref t) => quote! { #option::Some(#t) },
            None => quote! { #option::None },
        });
    }
}

fn box_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::boxed::Box }

    #[cfg(not(feature = "std"))]
    quote! { ::alloc::boxed::Box }
}

fn result_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::result::Result }

    #[cfg(not(feature = "std"))]
    quote! { ::core::result::Result }
}

fn option_type() -> TokenStream {
    #[cfg(feature = "std")]
    quote! { ::std::option::Option }

    #[cfg(not(feature = "std"))]
    quote! { ::core::option::Option }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_simple() {
        let rules = vec![OptimizedRule {
            name: "f".to_owned(),
            ty: RuleType::Normal,
            expr: OptimizedExpr::Ident("g".to_owned()),
        }];

        assert_eq!(
            generate_enum(&rules, false).to_string(),
            quote! {
                #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    f
                }
            }
            .to_string()
        );
    }

    #[test]
    fn sequence() {
        let expr = OptimizedExpr::Seq(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned())),
                )),
            )),
        );

        assert_eq!(
            generate_expr(expr).to_string(),
            quote! {
                state.sequence(|state| {
                    state.match_string("a").and_then(|state| {
                        super::hidden::skip(state)
                    }).and_then(|state| {
                        state.match_string("b")
                    }).and_then(|state| {
                        super::hidden::skip(state)
                    }).and_then(|state| {
                        state.match_string("c")
                    }).and_then(|state| {
                        super::hidden::skip(state)
                    }).and_then(|state| {
                        state.match_string("d")
                    })
                })
            }
            .to_string()
        );
    }

    #[test]
    fn sequence_atomic() {
        let expr = OptimizedExpr::Seq(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned())),
                )),
            )),
        );

        assert_eq!(
            generate_expr_atomic(expr).to_string(),
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
            .to_string()
        );
    }

    #[test]
    fn choice() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Choice(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Choice(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned())),
                )),
            )),
        );

        assert_eq!(
            generate_expr(expr).to_string(),
            quote! {
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
                    state.match_string("d")
                })
            }
            .to_string()
        );
    }

    #[test]
    fn choice_atomic() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Choice(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Choice(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned())),
                )),
            )),
        );

        assert_eq!(
            generate_expr_atomic(expr).to_string(),
            quote! {
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
                    state.match_string("d")
                })
            }
            .to_string()
        );
    }

    #[test]
    fn skip() {
        let expr = OptimizedExpr::Skip(vec!["a".to_owned(), "b".to_owned()]);

        assert_eq!(
            generate_expr_atomic(expr).to_string(),
            quote! {
                let strings = ["a", "b"];

                state.skip_until(&strings)
            }
            .to_string()
        );
    }

    #[test]
    fn expr_complex() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Ident("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Range("a".to_owned(), "b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::NegPred(Box::new(OptimizedExpr::Rep(
                        Box::new(OptimizedExpr::Insens("b".to_owned())),
                    )))),
                    Box::new(OptimizedExpr::PosPred(Box::new(OptimizedExpr::Opt(
                        Box::new(OptimizedExpr::Rep(Box::new(OptimizedExpr::Choice(
                            Box::new(OptimizedExpr::Str("c".to_owned())),
                            Box::new(OptimizedExpr::Str("d".to_owned())),
                        )))),
                    )))),
                )),
            )),
        );

        let sequence = quote! {
            state.sequence(|state| {
                super::hidden::skip(state).and_then(
                    |state| {
                        state.match_insensitive("b")
                    }
                )
            })
        };
        let repeat = quote! {
            state.repeat(|state| {
                state.sequence(|state| {
                    super::hidden::skip(state).and_then(|state| {
                        state.match_string("c")
                            .or_else(|state| {
                                state.match_string("d")
                            })
                     })
                })
            })
        };
        assert_eq!(
            generate_expr(expr).to_string(),
            quote! {
                self::a(state).or_else(|state| {
                    state.sequence(|state| {
                        state.match_range('a'..'b').and_then(|state| {
                            super::hidden::skip(state)
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
                            super::hidden::skip(state)
                        }).and_then(|state| {
                            state.lookahead(true, |state| {
                                state.optional(|state| {
                                    state.sequence(|state| {
                                        state.optional(|state| {
                                            state.match_string("c")
                                            .or_else(|state| {
                                                state.match_string("d")
                                            }).and_then(|state| {
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
            .to_string()
        );
    }

    #[test]
    fn expr_complex_atomic() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Ident("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Range("a".to_owned(), "b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::NegPred(Box::new(OptimizedExpr::Rep(
                        Box::new(OptimizedExpr::Insens("b".to_owned())),
                    )))),
                    Box::new(OptimizedExpr::PosPred(Box::new(OptimizedExpr::Opt(
                        Box::new(OptimizedExpr::Rep(Box::new(OptimizedExpr::Choice(
                            Box::new(OptimizedExpr::Str("c".to_owned())),
                            Box::new(OptimizedExpr::Str("d".to_owned())),
                        )))),
                    )))),
                )),
            )),
        );

        assert_eq!(
            generate_expr_atomic(expr).to_string(),
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
            .to_string()
        );
    }

    #[test]
    fn generate_complete() {
        let name = Ident::new("MyParser", Span::call_site());
        let generics = Generics::default();
        let rules = vec![OptimizedRule {
            name: "a".to_owned(),
            ty: RuleType::Silent,
            expr: OptimizedExpr::Str("b".to_owned()),
        }];
        let defaults = vec!["ANY"];
        let result = result_type();
        let box_ty = box_type();
        let mut current_dir = std::env::current_dir().expect("Unable to get current directory");
        current_dir.push("test.pest");
        let test_path = current_dir.to_str().expect("path contains invalid unicode");
        assert_eq!(
            generate(name, &generics, Some(PathBuf::from("test.pest")), rules, defaults, true).to_string(),
            quote! {
                #[allow(non_upper_case_globals)]
                const _PEST_GRAMMAR_MyParser: &'static str = include_str!(#test_path);

                #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    a
                }

                #[allow(clippy::all)]
                impl ::pest::Parser<Rule> for MyParser {
                    fn parse<'i>(
                        rule: Rule,
                        input: &'i str
                    ) -> #result<
                        ::pest::iterators::Pairs<'i, Rule>,
                        ::pest::error::Error<Rule>
                    > {
                        mod rules {
                            #![allow(clippy::upper_case_acronyms)]
                            pub mod hidden {
                                use super::super::Rule;

                                #[inline]
                                #[allow(dead_code, non_snake_case, unused_variables)]
                                pub fn skip(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                                    Ok(state)
                                }
                            }

                            pub mod visible {
                                use super::super::Rule;

                                #[inline]
                                #[allow(non_snake_case, unused_variables)]
                                pub fn a(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                                    state.match_string("b")
                                }

                                #[inline]
                                #[allow(dead_code, non_snake_case, unused_variables)]
                                pub fn ANY(state: #box_ty<::pest::ParserState<'_, Rule>>) -> ::pest::ParseResult<#box_ty<::pest::ParserState<'_, Rule>>> {
                                    state.skip(1)
                                }
                            }

                            pub use self::visible::*;
                        }

                        ::pest::state(input, |state| {
                            match rule {
                                Rule::a => rules::a(state)
                            }
                        })
                    }
                }
            }.to_string()
        );
    }
}
