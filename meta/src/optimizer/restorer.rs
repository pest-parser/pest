// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
use std::collections::HashMap;

use optimizer::*;

pub fn restore_on_err(
    rule: OptimizedRule,
    rules_to_exprs: &HashMap<String, OptimizedExpr>
) -> OptimizedRule {
    match rule {
        OptimizedRule { name, ty, expr } => {
            let expr = expr.map_bottom_up(|expr| wrap_branching_exprs(expr, rules_to_exprs));

            OptimizedRule { name, ty, expr }
        }
    }
}

fn wrap_branching_exprs(
    expr: OptimizedExpr,
    rules_to_exprs: &HashMap<String, OptimizedExpr>
) -> OptimizedExpr {
    match expr {
        OptimizedExpr::Opt(expr) => {
            if child_modifies_state(&expr, rules_to_exprs) {
                OptimizedExpr::Opt(Box::new(OptimizedExpr::RestoreOnErr(expr)))
            } else {
                OptimizedExpr::Opt(expr)
            }
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let wrapped_lhs = if child_modifies_state(&lhs, rules_to_exprs) {
                Box::new(OptimizedExpr::RestoreOnErr(lhs))
            } else {
                lhs
            };
            let wrapped_rhs = if child_modifies_state(&rhs, rules_to_exprs) {
                Box::new(OptimizedExpr::RestoreOnErr(rhs))
            } else {
                rhs
            };
            OptimizedExpr::Choice(wrapped_lhs, wrapped_rhs)
        }
        OptimizedExpr::Rep(expr) => {
            if child_modifies_state(&expr, rules_to_exprs) {
                OptimizedExpr::Rep(Box::new(OptimizedExpr::RestoreOnErr(expr)))
            } else {
                OptimizedExpr::Rep(expr)
            }
        }
        _ => expr
    }
}

fn child_modifies_state(
    expr: &OptimizedExpr,
    rules_to_exprs: &HashMap<String, OptimizedExpr>
) -> bool {
    expr.iter_top_down().any(|expr| match expr {
        OptimizedExpr::Push(_) => true,
        OptimizedExpr::Ident(ref s) if s == "pop" => true,
        OptimizedExpr::Ident(ref name) => {
            let mut map = rules_to_exprs.clone();
            match map.remove(name) {
                Some(rule_expr) => child_modifies_state(&rule_expr, &map),
                None => false
            }
        }
        _ => false
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use optimizer::OptimizedExpr::*;

    #[test]
    fn restore_no_stack_children() {
        let rules = vec![
            OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Opt(Box::new(Str(String::from("a"))))
            },
        ];

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(
            restore_on_err(rules[0].clone(), rules_to_map),
            rules[0].clone()
        );
    }

    #[test]
    fn restore_with_child_stack_ops() {
        let rules = vec![
            OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Rep(Box::new(Push(Box::new(Str(String::from("a"))))))
            },
        ];

        let restored = OptimizedRule {
            name: "rule".to_owned(),
            ty: RuleType::Normal,
            expr: Rep(Box::new(RestoreOnErr(Box::new(Push(Box::new(Str(
                String::from("a")
            )))))))
        };

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(restore_on_err(rules[0].clone(), rules_to_map), restored);
    }

    #[test]
    fn restore_choice_branch_with_and_branch_without() {
        let rules = vec![
            OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: Choice(
                    Box::new(Push(Box::new(Str(String::from("a"))))),
                    Box::new(Str(String::from("a")))
                )
            },
        ];

        let restored = OptimizedRule {
            name: "rule".to_owned(),
            ty: RuleType::Normal,
            expr: Choice(
                Box::new(RestoreOnErr(Box::new(Push(Box::new(Str(String::from(
                    "a"
                ))))))),
                Box::new(Str(String::from("a")))
            )
        };

        let rules_to_map = &populate_rules_to_exprs(&rules);

        assert_eq!(restore_on_err(rules[0].clone(), rules_to_map), restored);
    }
}
