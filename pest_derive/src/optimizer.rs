// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::ast::*;

pub fn optimize(rules: Vec<Rule>) -> Vec<Rule> {
    rules.into_iter().map(|rule| {
        let rotate_right = |expr| {
            match expr {
                Expr::Seq(lhs, rhs) => {
                    let lhs = *lhs;
                    match lhs {
                        Expr::Seq(ll, lr) => {
                            Expr::Seq(
                                ll,
                                Box::new(Expr::Seq(lr, rhs))
                            )
                        }
                        lhs => Expr::Seq(Box::new(lhs), rhs)
                    }
                }
                Expr::Choice(lhs, rhs) => {
                    let lhs = *lhs;
                    match lhs {
                        Expr::Choice(ll, lr) => {
                            Expr::Choice(
                                ll,
                                Box::new(Expr::Choice(lr, rhs))
                            )
                        }
                        lhs => Expr::Choice(Box::new(lhs), rhs)
                    }
                }
                expr => expr
            }
        };

        match rule {
            Rule { name, ty, expr } => Rule {
                name,
                ty,
                expr: expr.map_bottom_up(rotate_right).map_bottom_up(|expr| {
                    if ty == RuleType::Atomic {
                        match expr {
                            Expr::Seq(lhs, rhs) => {
                                match (*lhs, *rhs) {
                                    (Expr::Str(lhs), Expr::Str(rhs)) => Expr::Str(lhs + &rhs),
                                    (Expr::Insens(lhs), Expr::Insens(rhs)) => {
                                        Expr::Insens(lhs + &rhs)
                                    }
                                    (lhs, rhs) => Expr::Seq(Box::new(lhs), Box::new(rhs))
                                }
                            }
                            expr => expr
                        }
                    } else {
                        expr
                    }
                }).map_top_down(|expr| {
                    match expr {
                        Expr::Choice(lhs, rhs) => {
                            match (*lhs, *rhs) {
                                (Expr::Seq(l1, r1), Expr::Seq(l2, r2)) => {
                                    if l1 == l2 {
                                        Expr::Seq(
                                            l1,
                                            Box::new(Expr::Choice(
                                                r1,
                                                r2
                                            ))
                                        )
                                    } else {
                                        Expr::Choice(
                                            Box::new(Expr::Seq(l1, r1)),
                                            Box::new(Expr::Seq(l2, r2))
                                        )
                                    }
                                }
                                (lhs, rhs) => Expr::Choice(Box::new(lhs), Box::new(rhs))
                            }
                        }
                        expr => expr
                    }
                })
            }
        }
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    use quote::Ident;

    #[test]
    fn concat_strings() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Seq(
                    Box::new(Expr::Seq(
                        Box::new(Expr::Str("a".to_owned())),
                        Box::new(Expr::Str("b".to_owned()))
                    )),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Str("c".to_owned())),
                        Box::new(Expr::Str("d".to_owned()))
                    ))
                )
            }
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Str("abcd".to_owned())
            }
        ];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn concat_insensitive_strings() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Seq(
                    Box::new(Expr::Seq(
                        Box::new(Expr::Insens("a".to_owned())),
                        Box::new(Expr::Insens("b".to_owned()))
                    )),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Insens("c".to_owned())),
                        Box::new(Expr::Insens("d".to_owned()))
                    ))
                )
            }
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Insens("abcd".to_owned())
            }
        ];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn long_common_sequence() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Choice(
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident(Ident::new("a"))),
                        Box::new(Expr::Seq(
                            Box::new(Expr::Ident(Ident::new("b"))),
                            Box::new(Expr::Ident(Ident::new("c")))
                        ))
                    )),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Seq(
                            Box::new(Expr::Ident(Ident::new("a"))),
                            Box::new(Expr::Ident(Ident::new("b")))
                        )),
                        Box::new(Expr::Ident(Ident::new("d")))
                    ))
                )
            }
        ];
        let optimized = vec![
            Rule {
                name: Ident::new("rule"),
                ty:   RuleType::Silent,
                expr: Expr::Seq(
                    Box::new(Expr::Ident(Ident::new("a"))),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident(Ident::new("b"))),
                        Box::new(Expr::Choice(
                            Box::new(Expr::Ident(Ident::new("c"))),
                            Box::new(Expr::Ident(Ident::new("d")))
                        ))
                    ))
                )
            }
        ];

        assert_eq!(optimize(rules), optimized);
    }
}
