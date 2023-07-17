// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::types::result_type;
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
pub use std::collections::BTreeMap as Map;
use std::collections::BTreeSet;

use crate::optimizer::OptimizedExpr;

fn ident(s: &str) -> Ident {
    format_ident!("r#{}", s)
}

fn rule_wrappers() -> TokenStream {
    quote! {
        super::rule_wrappers
    }
}

fn rule(name: &Ident, type_name: &TokenStream, rule_name: &Ident, doc: &String) -> TokenStream {
    let rule_wrappers = rule_wrappers();
    let result = result_type();
    quote! {
        #[doc = #doc]
        #[derive(Debug)]
        pub struct #name<'i> {
            #[doc = "Matched content."]
            pub content: #type_name,
        }
        impl<'i> ::pest::typed::RuleWrapper<super::Rule> for #name<'i> {
            const RULE: super::Rule = super::Rule::#rule_name;
        }
        impl<'i> ::core::ops::Deref for #name<'i> {
            type Target = #type_name;

            fn deref(&self) -> &Self::Target {
                &self.content
            }
        }
        impl<'i> ::pest::typed::TypeWrapper for #name<'i> {
            type Inner = #type_name;
        }
        impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for #name<'i>
        {
            #[inline]
            fn try_parse_with<const ATOMIC: bool, _Rule: ::pest::typed::RuleWrapper<super::Rule>>(
                input: ::pest::Position<'i>,
                stack: &mut ::pest::Stack<::pest::Span<'i>>,
            ) -> #result<(::pest::Position<'i>, Self), ::pest::error::Error<super::Rule>> {
                let (input, content) = #type_name::try_parse_with::<ATOMIC, #rule_wrappers::#rule_name>(input, stack)?;
                Ok((
                    input,
                    Self {
                        content,
                    },
                ))
            }
        }
        impl<'i> ::pest::typed::ParsableTypedNode<'i, super::Rule> for #name<'i>
        {
            #[inline]
            fn parse(input: &'i str) -> #result<Self, ::pest::error::Error<super::Rule>> {
                let mut stack = ::pest::Stack::new();
                let (input, res) =
                    Self::try_parse_with::<false, #rule_wrappers::#rule_name>(::pest::Position::from_start(input), &mut stack)?;
                let (_, _) = ::pest::typed::predefined_node::EOI::try_parse_with::<false, #rule_wrappers::EOI>(input, &mut stack)?;
                Ok(res)
            }
        }
    }
}

struct Output {
    content: Vec<TokenStream>,
    wrappers: Vec<TokenStream>,
}
impl Output {
    fn new() -> Self {
        Self {
            content: Vec::new(),
            wrappers: Vec::new(),
        }
    }
    fn insert(&mut self, tokens: TokenStream) {
        self.content.push(tokens);
    }
    /// Insert to wrapper module.
    /// Return the module path.
    fn insert_wrapper(&mut self, tokens: TokenStream) -> TokenStream {
        self.wrappers.push(tokens);
        quote! { __pest_string_wrapper }
    }
    fn collect(&self) -> TokenStream {
        let content = &self.content;
        let wrappers = &self.wrappers;
        quote! {
            #(#content)*
            pub mod __pest_string_wrapper {
                #(#wrappers)*
            }
        }
    }
}

/// Returns type name.
fn process_single_alias(
    map: &mut Output,
    expr: &OptimizedExpr,
    rule_name: &str,
    candidate_name: String,
    type_name: TokenStream,
    inner_spaces: Option<bool>,
    explicit: bool,
) -> TokenStream {
    let rule_name = ident(rule_name);
    let name = ident(&candidate_name);
    let type_name = match inner_spaces {
        Some(true) => {
            quote! {::pest::typed::predefined_node::NonAtomic::<'i, super::Rule, #type_name>}
        }
        Some(false) => {
            quote! {::pest::typed::predefined_node::Atomic::<'i, super::Rule, #type_name>}
        }
        None => type_name,
    };
    if explicit {
        let doc = format!("Corresponds to expression: `{}`.", expr);
        let def = rule(&name, &type_name, &rule_name, &doc);
        map.insert(def);
        quote! {#name::<'i>}
    } else {
        type_name
    }
}

/// Returns type name.
fn generate_graph_node(
    expr: &OptimizedExpr,
    rule_name: &str,
    candidate_name: String,
    // From node name to type definition and implementation
    map: &mut Output,
    explicit: bool,
    inner_spaces: Option<bool>,
    inner_tokens: bool,
    silent: bool,
) -> TokenStream {
    // Still some compile-time information not taken
    match expr {
        OptimizedExpr::Str(content) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            let doc = format!("A wrapper for `\"{}\"`.", content);
            let module = map.insert_wrapper(quote! {
                #[doc = #doc]
                pub struct #wrapper();
                impl ::pest::typed::StringWrapper for #wrapper {
                    const CONTENT: &'static str = #content;
                }
            });
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Str::<'i, super::Rule, #module::#wrapper>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Insens(content) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            let doc = format!("A wrapper for `\"{}\"`.", content);
            let module = map.insert_wrapper(quote! {
                pub struct #wrapper();
                #[doc = #doc]
                impl ::pest::typed::StringWrapper for #wrapper {
                    const CONTENT: &'static str = #content;
                }
            });
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Insens::<'i, super::Rule, #module::#wrapper>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::PeekSlice(start, end) => process_single_alias(
            map,
            expr,
            rule_name,
            candidate_name,
            match end {
                Some(end) => quote! {
                    ::pest::typed::predefined_node::PeekSlice2::<'i, super::Rule, #start, #end>
                },
                None => quote! {
                    ::pest::typed::predefined_node::PeekSlice1::<'i, super::Rule, #start>
                },
            },
            inner_spaces,
            explicit,
        ),
        OptimizedExpr::Push(expr) => {
            let inner = generate_graph_node(
                expr,
                rule_name,
                format! {"{}_p", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Push::<'i, super::Rule, #inner>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Skip(strings) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            let doc = format!("A wrapper for `\"{:?}\"`.", strings);
            let module = map.insert_wrapper(quote! {
                #[doc = #doc]
                pub struct #wrapper();
                impl ::pest::typed::StringArrayWrapper for #wrapper {
                    const CONTENT: &'static[&'static str] = &[ #(#strings),* ];
                }
            });
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Skip::<'i, super::Rule, #module::#wrapper>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Range::<'i, super::Rule, #start, #end>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Ident(id) => {
            let inner = ident(id);
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {::pest::typed::predefined_node::Box::<'i, super::Rule, #inner::<'i>>},
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::PosPred(expr) => {
            let inner = generate_graph_node(
                expr,
                rule_name,
                format! {"{}_P", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Positive::<'i, super::Rule, #inner>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::NegPred(expr) => {
            let inner = generate_graph_node(
                expr,
                rule_name,
                format! {"{}_N", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Negative::<'i, super::Rule, #inner>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let inner = generate_graph_node(
                expr,
                rule_name,
                format! {"{}_E", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Restorable::<'i, super::Rule, #inner>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let first = generate_graph_node(
                lhs,
                rule_name,
                format! {"{}_0", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let second = generate_graph_node(
                rhs,
                rule_name,
                format! {"{}_1", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Seq::<
                        'i,
                        super::Rule,
                        #first,
                        #second,
                        ::pest::typed::predefined_node::Ign::<
                            'i,
                            super::Rule,
                            COMMENT::<'i>,
                            WHITESPACE::<'i>,
                        >
                    >
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let first = generate_graph_node(
                lhs,
                rule_name,
                format! {"{}_0", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let second = generate_graph_node(
                rhs,
                rule_name,
                format! {"{}_1", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Choice::<
                        'i,
                        super::Rule,
                        #first,
                        #second,
                    >
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Opt(inner) => {
            let inner_name = generate_graph_node(
                inner,
                rule_name,
                format!("{}_o", candidate_name),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {::pest::typed::predefined_node::Opt::<'i, super::Rule, #inner_name>},
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Rep(inner) => {
            let inner_name = generate_graph_node(
                inner,
                rule_name,
                format!("{}_r", candidate_name),
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Rep::<
                        'i,
                        super::Rule,
                        #inner_name,
                        ::pest::typed::predefined_node::Ign::<
                            'i,
                            super::Rule,
                            COMMENT::<'i>,
                            WHITESPACE::<'i>,
                        >
                    >
                },
                inner_spaces,
                explicit,
            )
        }
        #[cfg(feature = "grammar-extras")]
        OptimizedExpr::NodeTag(inner_expr, _tag) => {
            generate_graph_node(
                inner_expr,
                rule_name,
                format!("{}_0", candidate_name),
                map,
                explicit,
                inner_spaces,
                inner_tokens,
                silent,
            );
            todo!()
        }
    }
}

fn generate_graph(rules: &[OptimizedRule]) -> Output {
    // println!("{:#?}", rules);
    let mut res = Output::new();
    for rule in rules.iter() {
        let rule_name = rule.name.as_str();
        let candidate_name = rule.name.clone();
        match rule.ty {
            RuleType::Normal => {
                generate_graph_node(
                    &rule.expr,
                    rule_name,
                    candidate_name,
                    &mut res,
                    true,
                    None,
                    true,
                    false,
                );
            }
            RuleType::Silent => {
                generate_graph_node(
                    &rule.expr,
                    rule_name,
                    candidate_name,
                    &mut res,
                    true,
                    None,
                    true,
                    true,
                );
            }
            RuleType::NonAtomic => {
                generate_graph_node(
                    &rule.expr,
                    rule_name,
                    candidate_name,
                    &mut res,
                    true,
                    Some(true),
                    true,
                    true,
                );
            }
            RuleType::CompoundAtomic => {
                generate_graph_node(
                    &rule.expr,
                    rule_name,
                    candidate_name,
                    &mut res,
                    true,
                    Some(false),
                    true,
                    false,
                );
            }
            RuleType::Atomic => {
                generate_graph_node(
                    &rule.expr,
                    rule_name,
                    candidate_name,
                    &mut res,
                    true,
                    Some(false),
                    false,
                    false,
                );
            }
        }
    }
    res
}

pub fn generate_typed_pair_from_rule(rules: &[OptimizedRule]) -> TokenStream {
    // let names: Vec<_> = rules.iter().map(|rule| &rule.name).collect();
    // eprintln!("{:#?}", names);
    let graph = generate_graph(rules);
    let as_wrapper = |name: &Ident| {
        quote! {
            pub struct #name;
            impl ::pest::typed::RuleWrapper<super::Rule> for #name {
                const RULE: super::Rule = super::Rule::#name;
            }
        }
    };
    let rule_wrappers = rules.iter().map(|rule| {
        let name = ident(rule.name.as_str());
        as_wrapper(&name)
    });
    let eoi = as_wrapper(&ident("EOI"));
    let pairs = graph.collect();
    let rule_names: BTreeSet<&str> = rules.iter().map(|rule| rule.name.as_str()).collect();
    let builtin = generate_builtin(&rule_names);
    // let names = rules.iter().map(|rule| format_ident!("r#{}", rule.name));
    let res = quote! {
        #[doc(hidden)]
        mod rule_wrappers {
            #(#rule_wrappers)*
            #eoi
        }
        #[doc = "Definitions of statically typed nodes generated by pest-generator."]
        pub mod pairs {
            use ::pest::typed::NeverFailedTypedNode as _;
            #builtin

            #pairs
        }
    };
    // println!("{}", res);
    res
}

pub fn generate_builtin(rule_names: &BTreeSet<&str>) -> TokenStream {
    let mut result = vec![quote! {
        use ::pest::typed::predefined_node::{ANY, SOI, EOI, NEWLINE, PEEK_ALL, DROP};
        use ::pest::typed::TypedNode as _;
    }];
    macro_rules! insert_builtin {
        ($name:literal, $def:expr) => {
            if !rule_names.contains($name) {
                result.push($def);
            }
        };
    }
    insert_builtin!(
        "WHITESPACE",
        quote! {
            pub type WHITESPACE<'i> = ::pest::typed::predefined_node::AlwaysFail::<'i>;
        }
    );
    insert_builtin!(
        "COMMENT",
        quote! {
            pub type COMMENT<'i> = ::pest::typed::predefined_node::AlwaysFail::<'i>;
        }
    );
    quote! {
        #(#result)*
    }
}
