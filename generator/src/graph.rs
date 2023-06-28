// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
pub use std::collections::BTreeMap as Map;
use std::ops::Deref;

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

macro_rules! walk {
    ($var:ident) => {{
        let mut nodes: Vec<&Box<OptimizedExpr>> = vec![lhs];
        let mut names = vec![generate_graph_node::<false>(
            lhs,
            format!("{}_0", candidate_name),
            map,
        )];
        let mut current: &Box<OptimizedExpr> = rhs;
        let mut i = 1usize;
        while let OptimizedExpr::$var(lhs, rhs) = current.deref() {
            nodes.push(lhs);
            names.push(generate_graph_node::<false>(
                lhs,
                format!("{}_{}", candidate_name, i),
                map,
            ));
            current = rhs;
            i += 1;
        }
        nodes.push(current);
        map.entry(candidate_name.clone())
            .or_insert(GraphNode::Sequence(names));
        quote_ident(&candidate_name)
    }};
}

fn generate_graph_node<const FORCED: bool>(
    expr: &OptimizedExpr,
    candidate_name: String,
    map: &mut Map<String, GraphNode>,
) -> TokenStream {
    let mut copy_if_forced = |candidate_name: String, type_name: TokenStream| -> TokenStream {
        if FORCED {
            map.insert(candidate_name.clone(), GraphNode::Single(type_name));
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
        OptimizedExpr::Ident(id) => quote_ident(id),
        OptimizedExpr::PosPred(_) | OptimizedExpr::NegPred(_) | OptimizedExpr::RestoreOnErr(_) => {
            copy_if_forced(candidate_name, quote! {()})
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            walk!(Seq)
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            walk!(Choice)
        }
        OptimizedExpr::Opt(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Option(inner_name));
            quote_ident(&candidate_name)
        }
        OptimizedExpr::Rep(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Repeated(inner_name));
            quote_ident(&candidate_name)
        }
        #[cfg(feature = "grammar-extras")]
        OptimizedExpr::NodeTag(inner_expr, _tag) => {
            generate_graph_node::<false>(inner_expr, format!("{}_0", candidate_name), map);
        }
    }
}

pub fn generate_graph(rules: &[OptimizedRule]) -> Map<String, GraphNode> {
    let mut res = Map::<String, GraphNode>::new();
    for rule in rules.iter() {
        match rule.ty {
            RuleType::Normal
            | RuleType::Silent
            | RuleType::NonAtomic
            | RuleType::CompoundAtomic => {
                generate_graph_node::<true>(&rule.expr, rule.name.clone(), &mut res);
            }
            RuleType::Atomic => {
                res.entry(rule.name.clone())
                    .or_insert(GraphNode::Single(quote!(::std::string::String)));
            }
        }
    }
    res
}
