// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::optimizer::OptimizedExpr;
use crate::types::{option_type, result_type, vec_type};
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
use std::collections::btree_map;
pub use std::collections::BTreeMap as Map;
use std::collections::BTreeSet;

fn ident(s: &str) -> Ident {
    format_ident!("r#{}", s)
}

fn rule_wrappers() -> TokenStream {
    quote! {
        super::rule_wrappers
    }
}

fn ignore() -> TokenStream {
    quote! {
        ::pest::typed::predefined_node::Ign::<
            'i,
            super::Rule,
            COMMENT::<'i>,
            WHITESPACE::<'i>,
        >
    }
}

struct Accesser {
    /// name -> [([path], type)]
    accessers: Map<String, Vec<(TokenStream, TokenStream)>>,
}
impl Accesser {
    pub fn new() -> Self {
        Self {
            accessers: Map::new(),
        }
    }
    pub fn from_item(name: String, vec: Vec<(TokenStream, TokenStream)>) -> Self {
        let mut res = Map::new();
        res.insert(name, vec);
        Self { accessers: res }
    }
    pub fn prepend(
        mut self,
        fn_path: impl Fn(TokenStream) -> TokenStream,
        fn_type: impl Fn(TokenStream) -> TokenStream,
    ) -> Self {
        for (name, vec) in self.accessers.iter_mut() {
            for (path, typed) in vec.iter_mut() {
                *path = fn_path(path.clone());
                *typed = fn_type(typed.clone());
            }
        }
        self
    }
    pub fn join(mut self, other: Accesser) -> Accesser {
        other.accessers.into_iter().for_each(|(name, vec)| {
            match self.accessers.entry(name.clone()) {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(vec);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let v = entry.get_mut();
                    v.extend(vec.into_iter());
                }
            }
        });
        self
    }
    pub fn collect(&self) -> TokenStream {
        let accessers = self.accessers.iter().map(|(name, vec)| {
            let (paths, types): (Vec<_>, Vec<_>) = vec.clone().into_iter().unzip();
            let name = ident(name.as_str());
            quote! {
                pub fn #name(&self) ->( #(#types),* ) {
                    ( #( self #paths ),* )
                }
            }
        });
        quote! {
            #(#accessers)*
        }
    }
}

fn rule(
    name: &Ident,
    type_name: &TokenStream,
    rule_name: &Ident,
    doc: &String,
    accessers: &Accesser,
) -> TokenStream {
    let rule_wrappers = rule_wrappers();
    let result = result_type();
    let position = quote! {::pest::Position};
    let stack = quote! {::pest::Stack};
    let error = quote! {::pest::error::Error};
    let ignore = ignore();
    let accessers = accessers.collect();
    quote! {
        #[doc = #doc]
        #[derive(Debug)]
        pub struct #name<'i> {
            #[doc = "Matched content."]
            pub content: #type_name,
        }
        impl<'i> #name<'i> {
            #accessers
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
        impl<'i> ::pest::typed::TypedNode<'i, super::Rule> for #name<'i> {
            #[inline]
            fn try_parse_with<const ATOMIC: bool, _Rule: ::pest::typed::RuleWrapper<super::Rule>>(
                input: #position<'i>,
                stack: &mut #stack<::pest::Span<'i>>,
            ) -> #result<(#position<'i>, Self), ::pest::typed::error::Tracker<'i, super::Rule>> {
                let (input, content) = #type_name::try_parse_with::<ATOMIC, #rule_wrappers::#rule_name>(input, stack)?;
                Ok(
                    (
                        input,
                        Self {
                            content,
                        },
                    )
                )
            }
        }
        impl<'i> ::pest::typed::ParsableTypedNode<'i, super::Rule> for #name<'i> {
            #[inline]
            fn parse(input: &'i str) -> #result<Self, #error<super::Rule>> {
                let mut stack = #stack::new();
                let (input, res) =
                    match Self::try_parse_with::<false, #rule_wrappers::#rule_name>(#position::from_start(input), &mut stack) {
                        Ok((input, res)) => (input, res),
                        Err(e) => return Err(e.collect()),
                    };
                let (input, _) = #ignore::parse_with::<false, #rule_wrappers::EOI>(input, &mut stack);
                let (_, _) = match EOI::try_parse_with::<false, #rule_wrappers::EOI>(input, &mut stack) {
                    Ok((input, res)) => (input, res),
                    Err(e) => return Err(e.collect()),
                };
                Ok(res)
            }

            #[inline]
            fn parse_partial(input: &'i str) -> #result<(#position<'i>, Self), #error<super::Rule>> {
                let mut stack = #stack::new();
                match Self::try_parse_with::<false, #rule_wrappers::#rule_name>(#position::from_start(input), &mut stack) {
                    Ok((input, res)) => Ok((input, res)),
                    Err(e) => return Err(e.collect()),
                }
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
    accessers: Accesser,
    inner_spaces: Option<bool>,
    explicit: bool,
) -> (TokenStream, Accesser) {
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
        let def = rule(&name, &type_name, &rule_name, &doc, &accessers);
        map.insert(def);
        (quote! {#name::<'i>}, accessers)
    } else {
        (type_name, accessers)
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
) -> (TokenStream, Accesser) {
    let ignore = ignore();
    let option = option_type();
    let vec = vec_type();
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
                Accesser::new(),
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
                Accesser::new(),
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
            Accesser::new(),
            inner_spaces,
            explicit,
        ),
        OptimizedExpr::Push(expr) => {
            let (inner, accesser) = generate_graph_node(
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
                accesser.prepend(|inner| quote! {.content #inner}, |inner| inner),
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
                Accesser::new(),
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
                Accesser::new(),
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
                Accesser::from_item(
                    id.clone(),
                    vec![(quote! {.content.deref()}, quote! {&#inner})],
                ),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::PosPred(expr) => {
            let (inner, accessers) = generate_graph_node(
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
                accessers.prepend(|inner| quote! {.content #inner}, |inner| inner),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::NegPred(expr) => {
            // Impossible to access inner tokens.
            let (inner, _) = generate_graph_node(
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
                Accesser::new(),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let (inner, accessers) = generate_graph_node(
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
                accessers.prepend(|inner| quote! {.content #inner}, |inner| inner),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let (first, acc_first) = generate_graph_node(
                lhs,
                rule_name,
                format! {"{}_0", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let (second, acc_second) = generate_graph_node(
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
                        #ignore
                    >
                },
                acc_first
                    .prepend(|inner| quote! {.first #inner}, |inner| inner)
                    .join(acc_second.prepend(|inner| quote! {.second #inner}, |inner| inner)),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let (first, acc_first) = generate_graph_node(
                lhs,
                rule_name,
                format! {"{}_0", candidate_name},
                map,
                false,
                inner_spaces,
                inner_tokens,
                silent,
            );
            let (second, acc_second) = generate_graph_node(
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
                acc_first
                    .prepend(
                        |inner| quote! {.get_first().as_ref().and_then(|e|Some(e #inner))},
                        |inner| quote! {#option<&#inner>},
                    )
                    .join(acc_second.prepend(
                        |inner| quote! {.get_second().as_ref().and_then(|e|Some(e #inner))},
                        |inner| quote! {#option<&#inner>},
                    )),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Opt(inner) => {
            let (inner_name, accessers) = generate_graph_node(
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
                accessers.prepend(
                    |inner| quote! {.content.as_ref().and_then(|e|Some(e #inner))},
                    |inner| quote! {#option<&#inner>},
                ),
                inner_spaces,
                explicit,
            )
        }
        OptimizedExpr::Rep(inner) => {
            let (inner_name, accessers) = generate_graph_node(
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
                        #ignore,
                    >
                },
                accessers.prepend(
                    |inner| quote! {.content.iter().map(|e|e #inner).collect::<#vec<_>>()},
                    |inner| quote! {#vec<#inner>},
                ),
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
        use std::ops::Deref as _;
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
