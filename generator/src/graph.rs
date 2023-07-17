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

fn fn_decl() -> TokenStream {
    let result = result_type();
    quote! {
        #[inline]
        #[allow(unused_variables)]
        fn try_parse_with<const ATOMIC: bool, Rule: ::pest::typed::RuleWrapper<super::Rule>>(
            input: ::pest::Position<'i>,
            stack: &mut ::pest::Stack<::pest::Span<'i>>
        ) -> #result<(::pest::Position<'i>, Self), ::pest::error::Error<super::Rule>>
    }
}

fn attributes() -> TokenStream {
    // quote! {#[allow(non_snake_case)]}
    quote! {
        #[derive(Debug)]
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
    fn insert_wrapper(&mut self, tokens: TokenStream) {
        self.wrappers.push(tokens);
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

fn process_single(
    map: &mut Output,
    rule_name: &str,
    candidate_name: String,
    type_name: TokenStream,
    fimpl: TokenStream,
    doc: &str,
    silent: bool,
) -> TokenStream {
    let f = fn_decl();
    let rule_name = ident(rule_name);
    let name = ident(&candidate_name);
    let inner = type_name.clone();
    let fn_def = if silent {
        quote! {
            #fimpl;
            Ok((input, Self { _phantom: ::core::marker::PhantomData }))
        }
    } else {
        quote! {
            #fimpl;
            Ok((input, Self { span, content }))
        }
    };
    let fields = if silent {
        quote! {
            _phantom: ::core::marker::PhantomData<::pest::Span<'i>>
        }
    } else {
        quote! {
            pub span: ::pest::Span<'i>,
            pub content: #inner,
        }
    };
    let debug = if silent {
        quote! {
            impl<'i> ::core::fmt::Debug for #name<'i> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(#candidate_name)
                        .finish()
                }
            }
        }
    } else {
        quote! {
            impl<'i> ::core::fmt::Debug for #name<'i> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(#candidate_name)
                        .field("span", &self.span)
                        .field("content", &self.content)
                        .finish()
                }
            }
        }
    };
    let def = quote! {
        #[doc = #doc]
        pub struct #name<'i> {
            #fields
        }
        impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for #name<'i> {
            #f {
                #fn_def
            }
        }
        #debug
    };
    map.insert(def);
    quote! {
        #name::<'i>
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
            quote! {::pest::typed::predefined_node::NonAtomic<'i, super::Rule, #type_name>}
        }
        Some(false) => {
            quote! {::pest::typed::predefined_node::Atomic<'i, super::Rule, #type_name>}
        }
        None => type_name,
    };
    let type_name = quote! {
        ::pest::typed::predefined_node::Rule<'i, super::Rule, super::rule_wrappers::#rule_name, super::rule_wrappers::EOI, #type_name>
    };
    if explicit {
        let doc = format!("Corresponds to expression: `{}`.", expr);
        let def = quote! {
            #[doc = #doc]
            pub type #name<'i> = #type_name;
        };
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
    macro_rules! walk_tree {
        ($ivar: ident, $ovar: ident) => {{
            let mut nodes: Vec<&OptimizedExpr> = Vec::new();
            let mut names: Vec<TokenStream> = Vec::new();
            let mut i = 0usize;
            let mut current = expr;
            let mut gen = |node: &OptimizedExpr| {
                let res = generate_graph_node(
                    node,
                    rule_name,
                    format!("{}_{}", candidate_name, i),
                    map,
                    false,
                    None,
                    inner_tokens,
                    silent,
                );
                i += 1;
                res
            };
            while let OptimizedExpr::$ivar(lhs, rhs) = current {
                nodes.push(lhs);
                names.push(gen(lhs));
                current = rhs;
            }
            nodes.push(current);
            names.push(gen(current));
            let name = ident(&candidate_name);
            (nodes, names, quote! {#name::<'i>})
        }};
    }

    let f = fn_decl();
    let attr = attributes();

    let spaces = match inner_spaces {
        Some(true) => quote! {
            let (next, _) = ::pest::typed::predefined_node::Ign::<'i, super::Rule, WHITESPACE, COMMENT>::parse_with::<false, Rule>(input, stack);
            input = next;
        },
        Some(false) => quote! {},
        None => quote! {
            let (next, _) = ::pest::typed::predefined_node::Ign::<'i, super::Rule, WHITESPACE, COMMENT>::parse_with::<ATOMIC, Rule>(input, stack);
            input = next;
        },
    };
    let ispaces = match inner_spaces {
        Some(true) => quote! {false},
        Some(false) => quote! {true},
        None => quote! {ATOMIC},
    };

    // Still some compile-time information not taken
    match expr {
        OptimizedExpr::Str(content) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            let doc = format!("A wrapper for `\"{}\"`.", content);
            map.insert_wrapper(quote! {
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
                    ::pest::typed::predefined_node::Str::<'i, super::Rule, __pest_string_wrapper::#wrapper>
                },
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Insens(content) => {
            let wrapper = format_ident!("r#{}", candidate_name);
            let doc = format!("A wrapper for `\"{}\"`.", content);
            map.insert_wrapper(quote! {
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
                    ::pest::typed::predefined_node::Insens::<'i, super::Rule, __pest_string_wrapper::#wrapper>
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
            map.insert_wrapper(quote! {
                #[doc = #doc]
                pub struct #wrapper();
                impl ::pest::typed::StringArrayWrapper for #wrapper {
                    const CONTENT: &'static[&'static str] = [ #(#strings),* ];
                }
            });
            process_single_alias(
                map,
                expr,
                rule_name,
                candidate_name,
                quote! {
                    ::pest::typed::predefined_node::Skip::<'i, super::Rule, __pest_string_wrapper::#wrapper>
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
                    ::pest::typed::predefined_node::Seq<
                        'i,
                        R,
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
                    ::pest::typed::predefined_node::Choice<
                        'i,
                        R,
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
    let rule_wrappers = rules.iter().map(|rule| {
        let name = ident(rule.name.as_str());
        quote! {
            struct #name();
            impl ::pest::typed::RuleWrapper<super::Rule> for #name {
                const RULE: super::Rule = super::Rule::#name;
            }
        }
    });
    let pairs = graph.collect();
    let rule_names: BTreeSet<&str> = rules.iter().map(|rule| rule.name.as_str()).collect();
    let builtin = generate_builtin(&rule_names);
    // let names = rules.iter().map(|rule| format_ident!("r#{}", rule.name));
    let res = quote! {
        #[doc = "Definitions of statically typed nodes generated by pest-generator."]
        pub mod pairs {
            use super::Rule;
            mod rule_wrappers {
                #(#rule_wrappers)*
            }
            mod pairs {
                use pest::typed::NeverFailedTypedNode as _;
                #builtin

                #pairs
            }
            pub use pairs::*;
        }
    };
    println!("{}", res);
    res
}

pub fn generate_builtin(rule_names: &BTreeSet<&str>) -> TokenStream {
    let mut result = vec![quote! {
        use ::pest::typed::predefined_node::{ANY, SOI, EOI, NEWLINE, PEEK_ALL, DROP};
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
            type WHITESPACE<'i> = ::pest::typed::predefined_node::AlwaysFail<'i>;
        }
    );
    insert_builtin!(
        "COMMENT",
        quote! {
            type COMMENT<'i> = ::pest::typed::predefined_node::AlwaysFail<'i>;
        }
    );
    quote! {
        #(#result)*
    }
}
