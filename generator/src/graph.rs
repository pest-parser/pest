// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::types::{option_type, vec_type};
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
pub use std::collections::BTreeMap as Map;

use crate::optimizer::OptimizedExpr;

#[derive(Debug)]
pub enum GraphNode {
    Sequence(Vec<TokenStream>),
    Variant(Vec<TokenStream>),
    Single(TokenStream),
    Option(TokenStream),
    Repeated(TokenStream),
}

fn ident(s: &str) -> Ident {
    format_ident!("r#{}", s)
}
fn quote_ident(s: &str) -> TokenStream {
    let ident = ident(s);
    quote! {#ident}
}

macro_rules! walk_tree {
    ($ivar: ident, $ovar: ident, $expr: ident, $map: ident, $candidate_name: ident) => {{
        let mut nodes: Vec<&OptimizedExpr> = Vec::new();
        let mut names: Vec<TokenStream> = Vec::new();
        let mut i = 0usize;
        let mut current = $expr;
        while let OptimizedExpr::$ivar(lhs, rhs) = current {
            nodes.push(lhs);
            names.push(generate_graph_node::<false>(
                lhs,
                format!("{}_{}", $candidate_name, i),
                $map,
            ));
            current = rhs;
            i += 1;
        }
        nodes.push(current);
        $map.entry($candidate_name.clone())
            .or_insert((&$expr, GraphNode::$ovar(names)));
        quote_ident(&$candidate_name)
    }};
}

fn generate_graph_node<const FORCED: bool>(
    expr: &OptimizedExpr,
    candidate_name: String,
    map: &mut Map<String, (&OptimizedExpr, GraphNode)>,
) -> TokenStream {
    let mut copy_if_forced = |candidate_name: String, type_name: TokenStream| -> TokenStream {
        if FORCED {
            map.insert(
                candidate_name.clone(),
                (&expr, GraphNode::Single(type_name)),
            );
            quote_ident(&candidate_name)
        } else {
            type_name
        }
    };
    // Still some compile-time information not taken
    match expr {
        OptimizedExpr::Str(_)
        | OptimizedExpr::Insens(_)
        | OptimizedExpr::PeekSlice(_, _)
        | OptimizedExpr::Push(_)
        | OptimizedExpr::Skip(_) => copy_if_forced(candidate_name, quote! {::std::string::String}),
        OptimizedExpr::Range(_, _) => {
            copy_if_forced(candidate_name, quote! {::std::primitive::char})
        }
        OptimizedExpr::Ident(id) => copy_if_forced(candidate_name, quote_ident(id)),
        OptimizedExpr::PosPred(_) | OptimizedExpr::NegPred(_) | OptimizedExpr::RestoreOnErr(_) => {
            copy_if_forced(candidate_name, quote! {()})
        }
        OptimizedExpr::Seq(_lhs, _rhs) => walk_tree!(Seq, Sequence, expr, map, candidate_name),
        OptimizedExpr::Choice(_lhs, _rhs) => walk_tree!(Choice, Variant, expr, map, candidate_name),
        OptimizedExpr::Opt(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert((&expr, GraphNode::Option(inner_name)));
            quote_ident(&candidate_name)
        }
        OptimizedExpr::Rep(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert((&expr, GraphNode::Repeated(inner_name)));
            quote_ident(&candidate_name)
        }
        #[cfg(feature = "grammar-extras")]
        OptimizedExpr::NodeTag(inner_expr, _tag) => {
            generate_graph_node::<false>(inner_expr, format!("{}_0", candidate_name), map)
        }
    }
}

pub fn generate_graph(rules: &[OptimizedRule]) -> Map<String, (&OptimizedExpr, GraphNode)> {
    let mut res = Map::<String, (&OptimizedExpr, GraphNode)>::new();
    for rule in rules.iter() {
        match rule.ty {
            RuleType::Normal
            | RuleType::Silent
            | RuleType::NonAtomic
            | RuleType::CompoundAtomic => {
                generate_graph_node::<true>(&rule.expr, rule.name.clone(), &mut res);
            }
            RuleType::Atomic => {
                // eprintln!("{}", rule.name);
                res.entry(rule.name.clone())
                    .or_insert((&rule.expr, GraphNode::Single(quote!(::std::string::String))));
            }
        }
    }
    // eprintln!("{:?}", res.keys());
    // eprintln!("{:#?}", rules);
    res
}

pub fn generate_typed_pair_from_rule(rules: &[OptimizedRule]) -> TokenStream {
    let graph = generate_graph(rules);
    let ident = |s: &String| -> Ident { format_ident!("r#{}", s) };
    let pairs = graph.iter().map(|(name, rule)| {
        let name = ident(name);
        let f = quote!{
            #[inline]
            fn try_from(input: &'i str, line_index: ::pest::iterators::Option<::pest::iterators::Rc<::pest::iterators::line_index::LineIndex>>) -> Result<Self, ::pest::error::Error<R>>
        };
        match rule.1 {
            crate::graph::GraphNode::Sequence(inner) => {
                let fields = inner.iter().map(|i| i);
                let init = inner.iter().enumerate().map(|(i, field)| {
                    let name = format_ident!("r#field{}", i);
                    quote!{ let (input, #name) = #field::try_from(input, line_index)?;  }
                });
                quote! {
                    pub struct #name(#(#fields),*);
                    impl<'i, R: ::pest::RuleType> ::pest::iterators::TypedNode<'i, R> for #name {
                        #f {
                            #(#init)*
                            Ok(#(#inner),*)
                        }
                    }
                }
            }
            crate::graph::GraphNode::Variant(inner) => {
                let (names, vars): (Vec<_>, Vec<_>) = inner
                    .iter()
                    .enumerate()
                    .map(|(i, n)| (format_ident!("var_{}", i), n))
                    .unzip();
                let init = names.iter().zip(vars.iter()).map(|(name, var)| {
                    quote! {
                        if let (input, res) = var::try_from(input, line_index) {
                            return Ok((input, res));
                        }
                    }
                });
                quote! {
                    pub enum #name {
                        #( #names(#vars) ),*
                    }
                    impl<'i, R: ::pest::RuleType> ::pest::iterators::TypedNode<'i, R> for #name {
                        #f {
                            #(#init)*
                            panic!("All branches failed.");
                        }
                    }
                }
            }
            crate::graph::GraphNode::Single(inner) => {
                quote! {
                    pub struct #name(#inner);
                    impl<'i, R: ::pest::RuleType> ::pest::iterators::TypedNode<'i, R> for #name {
                        #f {
                            todo!()
                        }
                    }
                }
            }
            crate::graph::GraphNode::Option(inner) => {
                let option = option_type();
                quote! {
                    pub struct #name(#option::<#inner>);
                    impl<'i, R: ::pest::RuleType> ::pest::iterators::TypedNode<'i, R> for #name {
                        #f {
                            todo!()
                        }
                    }
                }
            }
            crate::graph::GraphNode::Repeated(inner) => {
                let vec = vec_type();
                quote! {
                    pub struct #name(#vec::<#inner>);
                    impl<'i, R: ::pest::RuleType> ::pest::iterators::TypedNode<'i, R> for #name {
                        #f {
                            todo!()
                        }
                    }
                }
            }
        }
    });
    // let names = rules.iter().map(|rule| format_ident!("r#{}", rule.name));
    let res = quote! {
        pub mod pairs {
            pub type ANY = char;
            pub type SOI = ();
            pub type EOI = ();
            pub type NEWLINE = ();
            #( #pairs )*
        }
    };
    // eprintln!("{}", res);
    res
}
