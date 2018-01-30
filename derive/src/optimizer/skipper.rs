// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ast::*;

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
            _ => None
        }
    }

    match rule {
        Rule { name, ty, expr } => Rule {
            name,
            ty,
            expr: if ty == RuleType::Atomic {
                expr.map_top_down(|expr| {
                    // TODO: Use box syntax when it gets stabilized.
                    match expr.clone() {
                        Expr::Rep(expr) => match *expr.clone() {
                            Expr::Seq(lhs, rhs) => match (*lhs, *rhs) {
                                (Expr::NegPred(expr), Expr::Ident(ident)) => {
                                    if ident.as_ref() == "any" {
                                        if let Some(expr) = populate_choices(*expr, vec![]) {
                                            return expr;
                                        }
                                    }
                                }
                                _ => ()
                            }
                            _ => ()
                        }
                        _ => ()
                    };

                    expr
                })
            } else {
                expr
            }
        }
    }
}
