// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ast::*;

pub fn factor(rule: Rule) -> Rule {
    match rule {
        Rule { name, ty, expr } => Rule {
            name,
            ty,
            expr: expr.map_top_down(|expr| {
                // TODO: Use box syntax when it gets stabilized.
                match expr {
                    Expr::Choice(lhs, rhs) => match (*lhs, *rhs) {
                        (Expr::Seq(l1, r1), Expr::Seq(l2, r2)) => {
                            if l1 == l2 {
                                Expr::Seq(l1, Box::new(Expr::Choice(r1, r2)))
                            } else {
                                Expr::Choice(
                                    Box::new(Expr::Seq(l1, r1)),
                                    Box::new(Expr::Seq(l2, r2)),
                                )
                            }
                        }
                        (lhs, rhs) => Expr::Choice(Box::new(lhs), Box::new(rhs)),
                    },
                    expr => expr,
                }
            }),
        },
    }
}
