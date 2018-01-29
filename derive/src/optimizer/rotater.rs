// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ast::*;

pub fn rotate(rule: Rule) -> Rule {
    match rule {
        Rule { name, ty, expr } => Rule {
            name,
            ty,
            // TODO: Use box syntax when it gets stabilized.
            expr: expr.map_bottom_up(|expr| match expr {
                Expr::Seq(lhs, rhs) => {
                    let lhs = *lhs;
                    match lhs {
                        Expr::Seq(ll, lr) => Expr::Seq(ll, Box::new(Expr::Seq(lr, rhs))),
                        lhs => Expr::Seq(Box::new(lhs), rhs)
                    }
                }
                Expr::Choice(lhs, rhs) => {
                    let lhs = *lhs;
                    match lhs {
                        Expr::Choice(ll, lr) => {
                            Expr::Choice(ll, Box::new(Expr::Choice(lr, rhs)))
                        }
                        lhs => Expr::Choice(Box::new(lhs), rhs)
                    }
                }
                expr => expr
            })
        }
    }
}
