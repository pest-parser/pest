// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Different optimizations for pest's ASTs.

use crate::ast::*;
use std::collections::HashMap;

#[cfg(test)]
macro_rules! box_tree {
    ( $node:ident( $(                      $child:ident( $($args:tt)* )     ),+ ) ) => (
      $node      ( $( Box::new( box_tree!( $child      ( $($args   )* ) ) ) ),+ )
    );
    ($expr:expr) => ($expr);
}

mod concatenator;
mod factorizer;
mod lister;
mod restorer;
mod rotater;
mod skipper;
mod unroller;

/// Takes pest's ASTs and optimizes them
pub fn optimize(rules: Vec<Rule>) -> Vec<OptimizedRule> {
    let optimized: Vec<OptimizedRule> = rules
        .into_iter()
        .map(rotater::rotate)
        .map(skipper::skip)
        .map(unroller::unroll)
        .map(concatenator::concatenate)
        .map(factorizer::factor)
        .map(lister::list)
        .map(rule_to_optimized_rule)
        .collect();

    let rules = to_hash_map(&optimized);
    optimized
        .into_iter()
        .map(|rule| restorer::restore_on_err(rule, &rules))
        .collect()
}

fn rule_to_optimized_rule(rule: Rule) -> OptimizedRule {
    fn to_optimized(expr: Expr) -> OptimizedExpr {
        match expr {
            Expr::Str(string) => OptimizedExpr::Str(string),
            Expr::Insens(string) => OptimizedExpr::Insens(string),
            Expr::Range(start, end) => OptimizedExpr::Range(start, end),
            Expr::Ident(ident) => OptimizedExpr::Ident(ident),
            Expr::PeekSlice(start, end) => OptimizedExpr::PeekSlice(start, end),
            Expr::PosPred(expr) => OptimizedExpr::PosPred(Box::new(to_optimized(*expr))),
            Expr::NegPred(expr) => OptimizedExpr::NegPred(Box::new(to_optimized(*expr))),
            Expr::Seq(lhs, rhs) => {
                OptimizedExpr::Seq(Box::new(to_optimized(*lhs)), Box::new(to_optimized(*rhs)))
            }
            Expr::Choice(lhs, rhs) => {
                OptimizedExpr::Choice(Box::new(to_optimized(*lhs)), Box::new(to_optimized(*rhs)))
            }
            Expr::Opt(expr) => OptimizedExpr::Opt(Box::new(to_optimized(*expr))),
            Expr::Rep(expr) => OptimizedExpr::Rep(Box::new(to_optimized(*expr))),
            Expr::Skip(strings) => OptimizedExpr::Skip(strings),
            Expr::Push(expr) => OptimizedExpr::Push(Box::new(to_optimized(*expr))),
            #[cfg(feature = "grammar-extras")]
            Expr::NodeTag(expr, tag) => OptimizedExpr::NodeTag(Box::new(to_optimized(*expr)), tag),
            Expr::RepOnce(_)
            | Expr::RepExact(..)
            | Expr::RepMin(..)
            | Expr::RepMax(..)
            | Expr::RepMinMax(..) => unreachable!("No valid transformation to OptimizedRule"),
        }
    }

    OptimizedRule {
        name: rule.name,
        ty: rule.ty,
        expr: to_optimized(rule.expr),
    }
}

fn to_hash_map(rules: &[OptimizedRule]) -> HashMap<String, OptimizedExpr> {
    rules
        .iter()
        .map(|r| (r.name.clone(), r.expr.clone()))
        .collect()
}

/// The optimized version of the pest AST's `Rule`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OptimizedRule {
    /// The name of the rule.
    pub name: String,
    /// The type of the rule.
    pub ty: RuleType,
    /// The optimized expression of the rule.
    pub expr: OptimizedExpr,
}

/// The optimized version of the pest AST's `Expr`.
///
/// # Warning: Semantic Versioning
/// There may be non-breaking changes to the meta-grammar
/// between minor versions. Those non-breaking changes, however,
/// may translate into semver-breaking changes due to the additional variants
/// propaged from the `Rule` enum. This is a known issue and will be fixed in the
/// future (e.g. by increasing MSRV and non_exhaustive annotations).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OptimizedExpr {
    /// Matches an exact string, e.g. `"a"`
    Str(String),
    /// Matches an exact string, case insensitively (ASCII only), e.g. `^"a"`
    Insens(String),
    /// Matches one character in the range, e.g. `'a'..'z'`
    Range(String, String),
    /// Matches the rule with the given name, e.g. `a`
    Ident(String),
    /// Matches a custom part of the stack, e.g. `PEEK[..]`
    PeekSlice(i32, Option<i32>),
    /// Positive lookahead; matches expression without making progress, e.g. `&e`
    PosPred(Box<OptimizedExpr>),
    /// Negative lookahead; matches if expression doesn't match, without making progress, e.g. `!e`
    NegPred(Box<OptimizedExpr>),
    /// Matches a sequence of two expressions, e.g. `e1 ~ e2`
    Seq(Box<OptimizedExpr>, Box<OptimizedExpr>),
    /// Matches either of two expressions, e.g. `e1 | e2`
    Choice(Box<OptimizedExpr>, Box<OptimizedExpr>),
    /// Optionally matches an expression, e.g. `e?`
    Opt(Box<OptimizedExpr>),
    /// Matches an expression zero or more times, e.g. `e*`
    Rep(Box<OptimizedExpr>),
    /// Continues to match expressions until one of the strings in the `Vec` is found
    Skip(Vec<String>),
    /// Matches an expression and pushes it to the stack, e.g. `push(e)`
    Push(Box<OptimizedExpr>),
    /// Matches an expression and assigns a label to it, e.g. #label = exp
    #[cfg(feature = "grammar-extras")]
    NodeTag(Box<OptimizedExpr>, String),
    /// Restores an expression's checkpoint
    RestoreOnErr(Box<OptimizedExpr>),
}

impl OptimizedExpr {
    /// Returns a top-down iterator over the `OptimizedExpr`.
    pub fn iter_top_down(&self) -> OptimizedExprTopDownIterator {
        OptimizedExprTopDownIterator::new(self)
    }

    /// Applies `f` to the `OptimizedExpr` top-down.
    pub fn map_top_down<F>(self, mut f: F) -> OptimizedExpr
    where
        F: FnMut(OptimizedExpr) -> OptimizedExpr,
    {
        fn map_internal<F>(expr: OptimizedExpr, f: &mut F) -> OptimizedExpr
        where
            F: FnMut(OptimizedExpr) -> OptimizedExpr,
        {
            let expr = f(expr);

            match expr {
                OptimizedExpr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::PosPred(mapped)
                }
                OptimizedExpr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::NegPred(mapped)
                }
                OptimizedExpr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Seq(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Choice(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Rep(mapped)
                }
                OptimizedExpr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Opt(mapped)
                }
                OptimizedExpr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Push(mapped)
                }
                expr => expr,
            }
        }

        map_internal(self, &mut f)
    }

    /// Applies `f` to the `OptimizedExpr` bottom-up.
    pub fn map_bottom_up<F>(self, mut f: F) -> OptimizedExpr
    where
        F: FnMut(OptimizedExpr) -> OptimizedExpr,
    {
        fn map_internal<F>(expr: OptimizedExpr, f: &mut F) -> OptimizedExpr
        where
            F: FnMut(OptimizedExpr) -> OptimizedExpr,
        {
            let mapped = match expr {
                OptimizedExpr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::PosPred(mapped)
                }
                OptimizedExpr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::NegPred(mapped)
                }
                OptimizedExpr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Seq(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Choice(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Rep(mapped)
                }
                OptimizedExpr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Opt(mapped)
                }
                OptimizedExpr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Push(mapped)
                }
                expr => expr,
            };

            f(mapped)
        }

        map_internal(self, &mut f)
    }
}

/// A top-down iterator over an `OptimizedExpr`.
pub struct OptimizedExprTopDownIterator {
    current: Option<OptimizedExpr>,
    next: Option<OptimizedExpr>,
    right_branches: Vec<OptimizedExpr>,
}

impl OptimizedExprTopDownIterator {
    /// Creates a new top down iterator from an `OptimizedExpr`.
    pub fn new(expr: &OptimizedExpr) -> Self {
        let mut iter = OptimizedExprTopDownIterator {
            current: None,
            next: None,
            right_branches: vec![],
        };
        iter.iterate_expr(expr.clone());
        iter
    }

    fn iterate_expr(&mut self, expr: OptimizedExpr) {
        self.current = Some(expr.clone());
        match expr {
            OptimizedExpr::Seq(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            OptimizedExpr::Choice(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            OptimizedExpr::PosPred(expr)
            | OptimizedExpr::NegPred(expr)
            | OptimizedExpr::Rep(expr)
            | OptimizedExpr::Opt(expr)
            | OptimizedExpr::Push(expr) => {
                self.next = Some(*expr);
            }
            _ => {
                self.next = None;
            }
        }
    }
}

impl Iterator for OptimizedExprTopDownIterator {
    type Item = OptimizedExpr;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current.take();

        if let Some(expr) = self.next.take() {
            self.iterate_expr(expr);
        } else if let Some(expr) = self.right_branches.pop() {
            self.iterate_expr(expr);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rotate() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: box_tree!(Choice(
                    Choice(
                        Choice(Str(String::from("a")), Str(String::from("b"))),
                        Str(String::from("c"))
                    ),
                    Str(String::from("d"))
                )),
            }]
        };
        let rotated = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Normal,
                expr: box_tree!(Choice(
                    Str(String::from("a")),
                    Choice(
                        Str(String::from("b")),
                        Choice(Str(String::from("c")), Str(String::from("d")))
                    )
                )),
            }]
        };

        assert_eq!(optimize(rules), rotated);
    }

    #[test]
    fn skip() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Rep(Seq(
                    NegPred(Choice(Str(String::from("a")), Str(String::from("b")))),
                    Ident(String::from("ANY"))
                ))),
            }]
        };
        let skipped = vec![OptimizedRule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: OptimizedExpr::Skip(vec![String::from("a"), String::from("b")]),
        }];

        assert_eq!(optimize(rules), skipped);
    }

    #[test]
    fn concat_strings() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(
                    Seq(Str(String::from("a")), Str(String::from("b"))),
                    Seq(Str(String::from("c")), Str(String::from("d")))
                )),
            }]
        };
        let concatenated = vec![OptimizedRule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: OptimizedExpr::Str(String::from("abcd")),
        }];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn unroll_loop_exact() {
        let rules = vec![Rule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: Expr::RepExact(Box::new(Expr::Ident(String::from("a"))), 3),
        }];
        let unrolled = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(
                    Ident(String::from("a")),
                    Seq(Ident(String::from("a")), Ident(String::from("a")))
                )),
            }]
        };

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_max() {
        let rules = vec![Rule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: Expr::RepMax(Box::new(Expr::Str("a".to_owned())), 3),
        }];
        let unrolled = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(
                    Opt(Str(String::from("a"))),
                    Seq(Opt(Str(String::from("a"))), Opt(Str(String::from("a"))))
                )),
            }]
        };

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_min() {
        let rules = vec![Rule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: Expr::RepMin(Box::new(Expr::Str("a".to_owned())), 2),
        }];
        let unrolled = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(
                    Str(String::from("a")),
                    Seq(Str(String::from("a")), Rep(Str(String::from("a"))))
                )),
            }]
        };

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn unroll_loop_min_max() {
        let rules = vec![Rule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: Expr::RepMinMax(Box::new(Expr::Str("a".to_owned())), 2, 3),
        }];
        let unrolled = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                /* TODO possible room for improvement here:
                 * if the sequences were rolled out in the opposite
                 * order, we could further optimize the strings
                 * in cases like this.
                Str(String::from(("aa")),
                Opt(Str(String::from("a")))
                */
                expr: box_tree!(Seq(
                    Str(String::from("a")),
                    Seq(Str(String::from("a")), Opt(Str(String::from("a"))))
                )),
            }]
        };

        assert_eq!(optimize(rules), unrolled);
    }

    #[test]
    fn concat_insensitive_strings() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(
                    Seq(Insens(String::from("a")), Insens(String::from("b"))),
                    Seq(Insens(String::from("c")), Insens(String::from("d")))
                )),
            }]
        };
        let concatenated = vec![OptimizedRule {
            name: "rule".to_owned(),
            ty: RuleType::Atomic,
            expr: OptimizedExpr::Insens(String::from("abcd")),
        }];

        assert_eq!(optimize(rules), concatenated);
    }

    #[test]
    fn long_common_sequence() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Choice(
                    Seq(
                        Ident(String::from("a")),
                        Seq(Ident(String::from("b")), Ident(String::from("c")))
                    ),
                    Seq(
                        Seq(Ident(String::from("a")), Ident(String::from("b"))),
                        Ident(String::from("d"))
                    )
                )),
            }]
        };
        let optimized = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Seq(
                    Ident(String::from("a")),
                    Seq(
                        Ident(String::from("b")),
                        Choice(Ident(String::from("c")), Ident(String::from("d")))
                    )
                )),
            }]
        };

        assert_eq!(optimize(rules), optimized);
    }

    #[test]
    fn short_common_sequence() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Choice(
                    Seq(Ident(String::from("a")), Ident(String::from("b"))),
                    Ident(String::from("a"))
                )),
            }]
        };
        let optimized = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Atomic,
                expr: box_tree!(Seq(Ident(String::from("a")), Opt(Ident(String::from("b"))))),
            }]
        };

        assert_eq!(optimize(rules), optimized);
    }

    #[test]
    fn impossible_common_sequence() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Choice(
                    Ident(String::from("a")),
                    Seq(Ident(String::from("a")), Ident(String::from("b")))
                )),
            }]
        };
        let optimized = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Ident(String::from("a"))),
            }]
        };

        assert_eq!(optimize(rules), optimized);
    }

    #[test]
    fn lister() {
        let rules = {
            use crate::ast::Expr::*;
            vec![Rule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Seq(
                    Rep(Seq(Ident(String::from("a")), Ident(String::from("b")))),
                    Ident(String::from("a"))
                )),
            }]
        };
        let optimized = {
            use crate::optimizer::OptimizedExpr::*;
            vec![OptimizedRule {
                name: "rule".to_owned(),
                ty: RuleType::Silent,
                expr: box_tree!(Seq(
                    Ident(String::from("a")),
                    Rep(Seq(Ident(String::from("b")), Ident(String::from("a"))))
                )),
            }]
        };

        assert_eq!(optimize(rules), optimized);
    }
}
