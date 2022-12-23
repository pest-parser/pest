// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::ast::*;

fn convert_expr(expr: Box<Expr>) -> Option<String> {
    match *expr {
        Expr::Str(string) => Some(string),
        _ => None,
    }
}

pub fn concatenate(rule: Rule) -> Rule {
    let Rule { name, ty, expr } = rule;
    Rule {
        name,
        ty,
        expr: expr.map_bottom_up(|expr| {
            if ty == RuleType::Atomic {
                // TODO: Use box syntax when it gets stabilized.
                match expr {
                    Expr::Seq(lhs, rhs) => match (*lhs, *rhs) {
                        (Expr::Str(lhs), Expr::Str(rhs)) => Expr::Str(lhs + &rhs),
                        (Expr::Insens(lhs), Expr::Insens(rhs)) => {
                            let lhs_str = convert_expr(lhs.clone());
                            let rhs_str = convert_expr(rhs.clone());

                            if lhs_str.is_none() || rhs_str.is_none() {
                                return Expr::Seq(
                                    Box::new(Expr::Insens(lhs)),
                                    Box::new(Expr::Insens(rhs)),
                                );
                            }

                            Expr::Insens(Box::new(Expr::Str(lhs_str.unwrap() + &rhs_str.unwrap())))
                        }
                        (lhs, rhs) => Expr::Seq(Box::new(lhs), Box::new(rhs)),
                    },
                    expr => expr,
                }
            } else {
                expr
            }
        }),
    }
}
