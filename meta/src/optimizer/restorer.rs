// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
use std::collections::HashMap;

use ast::*;

pub fn restore_on_err(rule: Rule, rules_to_exprs: &HashMap<String, Expr>) -> Rule {
    match rule {
        Rule { name, ty, expr } => {
            let expr = expr.map_bottom_up(|expr| {
                wrap_branching_exprs(expr, rules_to_exprs)
            });

            Rule {
                name,
                ty,
                expr
            }
        }
    }
}

fn wrap_branching_exprs(expr: Expr, rules_to_exprs: &HashMap<String, Expr>) -> Expr {
    match expr {
        Expr::Opt(expr) => {
            if child_modifies_state(&expr, rules_to_exprs) {
                Expr::Opt(Box::new(Expr::RestoreOnErr(expr)))
            } else {
                Expr::Opt(expr)
            }
        }
        Expr::Choice(lhs, rhs) => {
            let wrapped_lhs = if child_modifies_state(&lhs, rules_to_exprs) {
                Box::new(Expr::RestoreOnErr(lhs))
            } else {
                lhs
            };
            let wrapped_rhs = if child_modifies_state(&rhs, rules_to_exprs) {
                Box::new(Expr::RestoreOnErr(rhs))
            } else {
                rhs
            };
            Expr::Choice(wrapped_lhs, wrapped_rhs)
        }
        Expr::Rep(expr) => {
            if child_modifies_state(&expr, rules_to_exprs) {
                Expr::Rep(Box::new(Expr::RestoreOnErr(expr)))
            } else {
                Expr::Rep(expr)
            }
        }
        _ => expr
    }
}

fn child_modifies_state(expr: &Expr, rules_to_exprs: &HashMap<String, Expr>) -> bool {
    expr.iter_top_down().any(|expr| {
        match expr {
            Expr::Push(_) => true,
            Expr::Ident(ref s) if s == "pop" => true,
            Expr::Ident(ref name) => {
                let mut map = rules_to_exprs.clone();
                match map.remove(name) {
                    Some(rule_expr) => child_modifies_state(&rule_expr, &map),
                    None => false
                }
            },
            _ => false
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::*;

    #[test]
    fn restore_no_stack_children() {
        let rules = vec![
            Rule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Expr::Opt(Box::new(Expr::Str(String::from("a"))))
            },
        ];

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(restore_on_err(rules[0].clone(), rules_to_map), rules[0].clone());
    }

    #[test]
    fn restore_with_child_stack_ops() {
        let rules = vec![
            Rule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Expr::Rep(
                    Box::new(Expr::Push(
                        Box::new(Expr::Str(String::from("a")))
                    ))
                )
            },
        ];

        let restored = Rule {
            name: "rule".to_owned(),
            ty: RuleType::Normal,
            expr: Expr::Rep(
                Box::new(Expr::RestoreOnErr(Box::new(Expr::Push(
                    Box::new(Expr::Str(String::from("a")))
                )))
            ))
        };

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(restore_on_err(rules[0].clone(), rules_to_map), restored);
    }

    #[test]
    fn restore_choice_branch_with_and_branch_without() {
        let rules = vec![
            Rule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Expr::Choice(
                    Box::new(Expr::Push(
                        Box::new(Expr::Str(String::from("a")))
                    )),
                    Box::new(Expr::Str(String::from("a")))
                )
            },
        ];

        let restored = Rule {
            name: "rule".to_owned(),
            ty: RuleType::Normal,
            expr: Expr::Choice(
                Box::new(Expr::RestoreOnErr(Box::new(Expr::Push(
                    Box::new(Expr::Str(String::from("a")))
                )))),
                Box::new(Expr::Str(String::from("a")))
            )
        };

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(restore_on_err(rules[0].clone(), rules_to_map), restored);
    }
}
