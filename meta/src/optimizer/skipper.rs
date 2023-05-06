// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::ast::*;

pub fn skip(rule: Rule) -> Rule {
    fn populate_choices(expr: Expr, mut choices: Vec<String>) -> Option<Expr> {
        match expr {
            Expr::Choice(lhs, rhs) => {
                if let Expr::Str(string) = *lhs {
                    choices.push(string);
                    populate_choices(*rhs, choices)
                } else {
                    None
                }
            }
            Expr::Str(string) => {
                choices.push(string);
                Some(Expr::Skip(choices))
            }
            _ => None,
        }
    }

    let Rule { name, ty, expr } = rule;
    Rule {
        name,
        ty,
        expr: if ty == RuleType::Atomic {
            expr.map_top_down(|expr| {
                if let Expr::Rep(expr) = expr.clone() {
                    if let Expr::Seq(lhs, rhs) = *expr {
                        if let (Expr::NegPred(expr), Expr::Ident(ident)) = (*lhs, *rhs) {
                            if ident == "ANY" {
                                if let Some(expr) = populate_choices(*expr, vec![]) {
                                    return expr;
                                }
                            }
                        }
                    }
                };

                expr
            })
        } else {
            expr
        },
    }
}
