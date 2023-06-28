// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
pub use std::collections::BTreeMap as Map;
use std::ops::Deref;

use crate::optimizer::OptimizedExpr;

pub enum GraphNode {
    Sequence(Vec<String>),
    Variant(Vec<String>),
    Single(String),
    Option(String),
    Repeated(String),
}

fn generate_graph_node<const FORCED: bool>(
    expr: &OptimizedExpr,
    candidate_name: String,
    map: &mut Map<String, GraphNode>,
) -> String {
    let mut copy_if_forced = |candidate_name: String, type_name: &'static str| -> String {
        if FORCED {
            map.insert(
                candidate_name.clone(),
                GraphNode::Single(type_name.to_string()),
            );
            candidate_name
        } else {
            type_name.to_string()
        }
    };
    // Still some compile-time information not taken
    match expr {
        OptimizedExpr::Str(_)
        | OptimizedExpr::Insens(_)
        | OptimizedExpr::PeekSlice(_, _)
        | OptimizedExpr::Push(_)
        | OptimizedExpr::Skip(_) => copy_if_forced(candidate_name, "::std::string::String"),
        OptimizedExpr::Range(_, _) => copy_if_forced(candidate_name, "::std::primitive::char"),
        OptimizedExpr::Ident(id) => id.to_string(),
        OptimizedExpr::PosPred(_) | OptimizedExpr::NegPred(_) | OptimizedExpr::RestoreOnErr(_) => {
            copy_if_forced(candidate_name, "()")
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let mut nodes: Vec<&Box<OptimizedExpr>> = vec![lhs];
            let mut names = vec![generate_graph_node::<false>(
                lhs,
                format!("{}_0", candidate_name),
                map,
            )];
            let mut current: &Box<OptimizedExpr> = rhs;
            while let OptimizedExpr::Seq(lhs, rhs) = current.deref() {
                nodes.push(lhs);
                names.push(generate_graph_node::<false>(
                    lhs,
                    format!("{}_0", candidate_name),
                    map,
                ));
                current = rhs;
            }
            nodes.push(current);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Sequence(names));
            candidate_name
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let mut nodes = vec![lhs];
            let mut current: &Box<OptimizedExpr> = rhs;
            let mut names = vec![generate_graph_node::<false>(
                lhs,
                format!("{}_0", candidate_name),
                map,
            )];
            while let OptimizedExpr::Seq(lhs, rhs) = current.deref() {
                nodes.push(lhs);
                names.push(generate_graph_node::<false>(
                    lhs,
                    format!("{}_0", candidate_name),
                    map,
                ));
                current = rhs;
            }
            nodes.push(current);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Sequence(names));
            candidate_name
        }
        OptimizedExpr::Opt(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Option(inner_name));
            candidate_name
        }
        OptimizedExpr::Rep(inner) => {
            let inner_name =
                generate_graph_node::<false>(inner, format!("{}_0", candidate_name), map);
            map.entry(candidate_name.clone())
                .or_insert(GraphNode::Repeated(inner_name));
            candidate_name
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
                    .or_insert(GraphNode::Sequence(vec![
                        "::std::string::String".to_string()
                    ]));
            }
        }
    }
    res
}
