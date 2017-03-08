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

fn to_hash_map(rules: Vec<Rule>) -> HashMap<Ident, Body> {
    let mut hash_map = HashMap::new();

    map_rules(rules, |rule| {
        if hash_map.contains_key(&rule.name) {
            panic!("Rule {} is defined multiple times", rule.name);
        } else {
            hash_map.insert(rule.name.clone(), rule.body.clone());
        }

        rule
    });

    hash_map
}

fn left_recursion(rules: HashMap<Ident, Body>) {
    fn check_expr(mut names: HashSet<Ident>, expr: &Expr, rules: &HashMap<Ident, Body>) {
        match expr {
            &Expr::Ident(ref other)  => {
                if names.contains(other) {
                    panic!("Rule {} is left-recursive", other);
                }

                names.insert(other.clone());

                check_body(names, rules.get(other).unwrap(), rules);
            },
            &Expr::Seq(ref exprs)    => check_expr(names, &exprs[0], rules),
            &Expr::Choice(ref exprs) => check_expr(names, &exprs[0], rules),
            &Expr::RepZero(ref expr) => check_expr(names, &expr, rules),
            &Expr::RepOne(ref expr)  => check_expr(names, &expr, rules),
            &Expr::Opt(ref expr)     => check_expr(names, &expr, rules),
            &Expr::PosLhd(ref expr)  => check_expr(names, &expr, rules),
            &Expr::NegLhd(ref expr)  => check_expr(names, &expr, rules),
            &Expr::Push(ref expr)    => check_expr(names, &expr, rules),
            _                        => ()
        }
    }

    fn check_body(names: HashSet<Ident>, body: &Body, rules: &HashMap<Ident, Body>) {
        match body {
            &Body::Normal(ref expr)   => check_expr(names, expr, rules),
            &Body::Infix(ref expr, _) => check_expr(names, expr, rules)
        }
    }

    for (name, body) in &rules {
        let mut names = HashSet::new();

        names.insert((*name).clone());

        check_body(names, body, &rules);
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
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Str("b".to_owned())
                )
            },
            Rule {
                name: Ident::new("a"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Str("b".to_owned())
                )
            },
        ];

        to_hash_map(rules);
    }

    #[test]
    fn no_duplicates() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Str("b".to_owned())
                )
            },
            Rule {
                name: Ident::new("c"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Str("b".to_owned())
                )
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
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Opt(Box::new(
                        Expr::RepZero(Box::new(
                            Expr::Ident(Ident::new("b"))
                        ))
                    ))
                )
            },
            Rule {
                name: Ident::new("b"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Ident(Ident::new("a"))
                )
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
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Opt(Box::new(
                        Expr::RepZero(Box::new(
                            Expr::Ident(Ident::new("b"))
                        ))
                    ))
                )
            },
            Rule {
                name: Ident::new("b"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Ident(Ident::new("b"))
                )
            },
        ];

        left_recursion(to_hash_map(rules));
    }

    #[test]
    fn non_left_recursive() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Opt(Box::new(
                        Expr::RepZero(Box::new(
                            Expr::Ident(Ident::new("b"))
                        ))
                    ))
                )
            },
            Rule {
                name: Ident::new("b"),
                ty:   RuleType::Normal,
                body: Body::Normal(
                    Expr::Str("c".to_owned())
                )
            },
        ];

        left_recursion(to_hash_map(rules));
    }

    #[test]
    #[should_panic(expected = "Rule a is defined multiple times")]
    fn duplicate_definition_nested() {
        let rules = vec![
            Rule {
                name: Ident::new("a"),
                ty:   RuleType::Normal,
                body: Body::Infix(
                    Expr::Str("b".to_owned()),
                    vec![
                        (Rule {
                            name: Ident::new("a"),
                            ty:   RuleType::Normal,
                            body: Body::Normal(
                                Expr::Str("b".to_owned())
                            )
                        }, false)
                    ]
                )
            },
        ];

        to_hash_map(rules);
    }
}
