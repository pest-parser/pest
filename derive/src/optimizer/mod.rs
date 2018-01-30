// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ast::*;

mod concatenator;
mod factorizer;
mod rotater;
mod skipper;
mod unroller;

pub fn optimize(rules: Vec<Rule>) -> Vec<Rule> {
    rules
        .into_iter()
        .map(rotater::rotate)
        .map(skipper::skip)
        .map(unroller::unroll)
        .map(concatenator::concatenate)
        .map(factorizer::factor)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    use quote::Ident;

    #[test]
    fn skip() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Rep(
                    Box::new(Expr::Seq(
                        Box::new(Expr::NegPred(
                            Box::new(Expr::Choice(
                                Box::new(Expr::Str("a".to_owned())),
                                Box::new(Expr::Str("b".to_owned()))
                            ))
                        )),
                        Box::new(Expr::Ident(Ident::new("any")))
                    ))
                )
            },
        ];
        let skipped = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Skip(vec!["a".to_owned(), "b".to_owned()])
            },
        ];

        assert_eq!(optimize(rules), skipped);
    }

    #[test]
    fn concat_strings() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
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
            },
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Str("abcd".to_owned())
            },
        ];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn unroll_loop_exact() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::RepExact(Box::new(Expr::Ident(Ident::new("a"))), 3)
            },
        ];
        let unrolled = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Seq(
                    Box::new(Expr::Ident(Ident::new("a"))),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Ident(Ident::new("a"))),
                        Box::new(Expr::Ident(Ident::new("a")))
                    ))
                )
            },
        ];

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_max() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::RepMax(Box::new(Expr::Str("a".to_owned())), 3)
            },
        ];
        let unrolled = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Seq(
                    Box::new(Expr::Opt(Box::new(Expr::Str("a".to_owned())))),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Opt(Box::new(Expr::Str("a".to_owned())))),
                        Box::new(Expr::Opt(Box::new(Expr::Str("a".to_owned()))))
                    ))
                )
            },
        ];

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_min() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::RepMin(Box::new(Expr::Str("a".to_owned())), 2)
            },
        ];
        let unrolled = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Seq(
                    Box::new(Expr::Str("a".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Str("a".to_owned())),
                        Box::new(Expr::Rep(Box::new(Expr::Str("a".to_owned()))))
                    ))
                )
            },
        ];

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_min_max() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::RepMinMax(Box::new(Expr::Str("a".to_owned())), 2, 3)
            },
        ];
        let unrolled = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Seq(
                    /* TODO possible room for improvement here:
                     * if the sequences were rolled out in the opposite
                     * order, we could further optimize the strings
                     * in cases like this.
                    Box::new(Expr::Str("aa".to_owned())),
                    Box::new(Expr::Opt(
                        Box::new(Expr::Str("a".to_owned()))
                    ))
                    */
                    Box::new(Expr::Str("a".to_owned())),
                    Box::new(Expr::Seq(
                        Box::new(Expr::Str("a".to_owned())),
                        Box::new(Expr::Opt(Box::new(Expr::Str("a".to_owned()))))
                    ))
                )
            },
        ];

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn concat_insensitive_strings() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
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
            },
        ];
        let concatenated = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Atomic,
                expr: Expr::Insens("abcd".to_owned())
            },
        ];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn long_common_sequence() {
        let rules = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Silent,
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
            },
        ];
        let optimized = vec![
            Rule {
                name: Ident::new("rule"),
                ty: RuleType::Silent,
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
            },
        ];

        assert_eq!(optimize(rules), optimized);
    }
}
