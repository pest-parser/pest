// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::ast::*;

pub fn factor(rule: Rule) -> Rule {
    let Rule { name, ty, expr } = rule;
    Rule {
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
                            Expr::Choice(Box::new(Expr::Seq(l1, r1)), Box::new(Expr::Seq(l2, r2)))
                        }
                    }
                    // Converts `(rule ~ rest) | rule` to `rule ~ rest?`, avoiding trying to match `rule` twice.
                    (Expr::Seq(l1, l2), r) => {
                        if *l1 == r {
                            Expr::Seq(l1, Box::new(Expr::Opt(l2)))
                        } else {
                            Expr::Choice(Box::new(Expr::Seq(l1, l2)), Box::new(r))
                        }
                    }
                    // Converts `rule | (rule ~ rest)` to `rule` since `(rule ~ rest)`
                    // will never match if `rule` didn't.
                    (l, Expr::Seq(r1, r2)) => {
                        if l == *r1 {
                            l
                        } else {
                            Expr::Choice(Box::new(l), Box::new(Expr::Seq(r1, r2)))
                        }
                    }
                    (lhs, rhs) => Expr::Choice(Box::new(lhs), Box::new(rhs)),
                },
                expr => expr,
            }
        }),
    }
}
