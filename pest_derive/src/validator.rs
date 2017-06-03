// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::{HashMap, HashSet};

use quote::Ident;

use super::ast::*;

pub fn validate(rules: Vec<Rule>) {
    left_recursion(to_hash_map(rules));
}

fn to_hash_map(rules: Vec<Rule>) -> HashMap<Ident, Expr> {
    let mut hash_map = HashMap::new();

    for rule in rules {
        if hash_map.contains_key(&rule.name) {
            panic!("Rule {} is defined multiple times", rule.name);
        } else {
            hash_map.insert(rule.name.clone(), rule.expr.clone());
        }
    }

    hash_map
}

fn left_recursion(rules: HashMap<Ident, Expr>) {
    fn check_expr(mut names: HashSet<Ident>, expr: &Expr, rules: &HashMap<Ident, Expr>) {
        match *expr {
            Expr::Ident(ref other)  => {
                if names.contains(other) {
                    panic!("Rule {} is left-recursive", other);
                }

                names.insert(other.clone());

                check_expr(names, rules.get(other).unwrap(), rules);
            },
            Expr::Seq(ref lhs, _) => check_expr(names, &lhs, rules),
            Expr::Choice(ref lhs, _) => check_expr(names, &lhs, rules),
            Expr::Rep(ref expr) => check_expr(names, &expr, rules),
            Expr::RepOnce(ref expr) => check_expr(names, &expr, rules),
            Expr::Opt(ref expr) => check_expr(names, &expr, rules),
            Expr::PosPred(ref expr) => check_expr(names, &expr, rules),
            Expr::NegPred(ref expr) => check_expr(names, &expr, rules),
            Expr::Push(ref expr) => check_expr(names, &expr, rules),
            _ => ()
        }
    }

    for (name, expr) in &rules {
        let mut names = HashSet::new();

        names.insert((*name).clone());

        check_expr(names, expr, &rules);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Rule a is defined multiple times")]
    fn duplicate_definition() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Str("b".to_owned())
            },
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Str("b".to_owned())
            },
        ];

        to_hash_map(rules);
    }

    #[test]
    fn no_duplicates() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Str("b".to_owned())
            },
            Rule {
                name: Ident::new("c"),
                ty: RuleType::Normal,
                expr: Expr::Str("b".to_owned())
            },
        ];

        to_hash_map(rules);
    }

    #[test]
    #[should_panic]
    fn a_left_recursive() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Opt(Box::new(
                    Expr::Rep(Box::new(
                        Expr::Ident(Ident::new("b"))
                    ))
                ))
            },
            Rule {
                name: Ident::new("b"),
                ty: RuleType::Normal,
                expr: Expr::Ident(Ident::new("a"))
            },
        ];

        left_recursion(to_hash_map(rules));
    }

    #[test]
    #[should_panic(expected = "Rule b is left-recursive")]
    fn b_left_recursive() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Opt(Box::new(
                    Expr::Rep(Box::new(
                        Expr::Ident(Ident::new("b"))
                    ))
                ))
            },
            Rule {
                name: Ident::new("b"),
                ty: RuleType::Normal,
                expr: Expr::Ident(Ident::new("b"))
            },
        ];

        left_recursion(to_hash_map(rules));
    }

    #[test]
    fn non_left_recursive() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty: RuleType::Normal,
                expr: Expr::Opt(Box::new(
                    Expr::Rep(Box::new(
                        Expr::Ident(Ident::new("b"))
                    ))
                ))
            },
            Rule {
                name: Ident::new("b"),
                ty: RuleType::Normal,
                expr: Expr::Str("c".to_owned())
            },
        ];

        left_recursion(to_hash_map(rules));
    }
}
