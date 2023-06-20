// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Types for the pest's abstract syntax tree.

/// A grammar rule
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    /// The name of the rule
    pub name: String,
    /// The rule's type (silent, atomic, ...)
    pub ty: RuleType,
    /// The rule's expression
    pub expr: Expr,
}

/// All possible rule types
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    /// The normal rule type
    Normal,
    /// Silent rules are just like normal rules
    /// — when run, they function the same way —
    /// except they do not produce pairs or tokens.
    /// If a rule is silent, it will never appear in a parse result.
    /// (their syntax is `_{ ... }`)
    Silent,
    /// atomic rule prevent implicit whitespace: inside an atomic rule,
    /// the tilde ~ means "immediately followed by",
    /// and repetition operators (asterisk * and plus sign +)
    /// have no implicit separation. In addition, all other rules
    /// called from an atomic rule are also treated as atomic.
    /// In an atomic rule, interior matching rules are silent.
    /// (their syntax is `@{ ... }`)
    Atomic,
    /// Compound atomic rules are similar to atomic rules,
    /// but they produce inner tokens as normal.
    /// (their syntax is `${ ... }`)
    CompoundAtomic,
    /// Non-atomic rules cancel the effect of atomic rules.
    /// (their syntax is `!{ ... }`)
    NonAtomic,
}

/// All possible rule expressions
///
/// # Warning: Semantic Versioning
/// There may be non-breaking changes to the meta-grammar
/// between minor versions. Those non-breaking changes, however,
/// may translate into semver-breaking changes due to the additional variants
/// propaged from the `Rule` enum. This is a known issue and will be fixed in the
/// future (e.g. by increasing MSRV and non_exhaustive annotations).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
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
    PosPred(Box<Expr>),
    /// Negative lookahead; matches if expression doesn't match, without making progress, e.g. `!e`
    NegPred(Box<Expr>),
    /// Matches a sequence of two expressions, e.g. `e1 ~ e2`
    Seq(Box<Expr>, Box<Expr>),
    /// Matches either of two expressions, e.g. `e1 | e2`
    Choice(Box<Expr>, Box<Expr>),
    /// Optionally matches an expression, e.g. `e?`
    Opt(Box<Expr>),
    /// Matches an expression zero or more times, e.g. `e*`
    Rep(Box<Expr>),
    /// Matches an expression one or more times, e.g. `e+`
    RepOnce(Box<Expr>),
    /// Matches an expression an exact number of times, e.g. `e{n}`
    RepExact(Box<Expr>, u32),
    /// Matches an expression at least a number of times, e.g. `e{n,}`
    RepMin(Box<Expr>, u32),
    /// Matches an expression at most a number of times, e.g. `e{,n}`
    RepMax(Box<Expr>, u32),
    /// Matches an expression a number of times within a range, e.g. `e{m, n}`
    RepMinMax(Box<Expr>, u32, u32),
    /// Continues to match expressions until one of the strings in the `Vec` is found
    Skip(Vec<String>),
    /// Matches an expression and pushes it to the stack, e.g. `push(e)`
    Push(Box<Expr>),
    /// Matches an expression and assigns a label to it, e.g. #label = exp
    #[cfg(feature = "grammar-extras")]
    NodeTag(Box<Expr>, String),
}

impl Expr {
    /// Returns the iterator that steps the expression from top to bottom.
    pub fn iter_top_down(&self) -> ExprTopDownIterator {
        ExprTopDownIterator::new(self)
    }

    /// Applies `f` to the expression and all its children (top to bottom).
    pub fn map_top_down<F>(self, mut f: F) -> Expr
    where
        F: FnMut(Expr) -> Expr,
    {
        fn map_internal<F>(expr: Expr, f: &mut F) -> Expr
        where
            F: FnMut(Expr) -> Expr,
        {
            let expr = f(expr);

            match expr {
                Expr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::PosPred(mapped)
                }
                Expr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NegPred(mapped)
                }
                Expr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Seq(mapped_lhs, mapped_rhs)
                }
                Expr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Choice(mapped_lhs, mapped_rhs)
                }
                Expr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Rep(mapped)
                }
                Expr::RepOnce(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepOnce(mapped)
                }
                Expr::RepExact(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepExact(mapped, max)
                }
                Expr::RepMin(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMin(mapped, num)
                }
                Expr::RepMax(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMax(mapped, num)
                }
                Expr::RepMinMax(expr, min, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMinMax(mapped, min, max)
                }
                Expr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Opt(mapped)
                }
                Expr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Push(mapped)
                }
                #[cfg(feature = "grammar-extras")]
                Expr::NodeTag(expr, tag) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NodeTag(mapped, tag)
                }
                expr => expr,
            }
        }

        map_internal(self, &mut f)
    }

    /// Applies `f` to the expression and all its children (bottom up).
    pub fn map_bottom_up<F>(self, mut f: F) -> Expr
    where
        F: FnMut(Expr) -> Expr,
    {
        fn map_internal<F>(expr: Expr, f: &mut F) -> Expr
        where
            F: FnMut(Expr) -> Expr,
        {
            let mapped = match expr {
                Expr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::PosPred(mapped)
                }
                Expr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NegPred(mapped)
                }
                Expr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Seq(mapped_lhs, mapped_rhs)
                }
                Expr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    Expr::Choice(mapped_lhs, mapped_rhs)
                }
                Expr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Rep(mapped)
                }
                Expr::RepOnce(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepOnce(mapped)
                }
                Expr::RepExact(expr, num) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepExact(mapped, num)
                }
                Expr::RepMin(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMin(mapped, max)
                }
                Expr::RepMax(expr, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMax(mapped, max)
                }
                Expr::RepMinMax(expr, min, max) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::RepMinMax(mapped, min, max)
                }
                Expr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Opt(mapped)
                }
                Expr::Push(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::Push(mapped)
                }
                #[cfg(feature = "grammar-extras")]
                Expr::NodeTag(expr, tag) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    Expr::NodeTag(mapped, tag)
                }
                expr => expr,
            };

            f(mapped)
        }

        map_internal(self, &mut f)
    }
}

/// The top down iterator for an expression.
pub struct ExprTopDownIterator {
    current: Option<Expr>,
    next: Option<Expr>,
    right_branches: Vec<Expr>,
}

impl ExprTopDownIterator {
    /// Constructs a top-down iterator from the expression.
    pub fn new(expr: &Expr) -> Self {
        let mut iter = ExprTopDownIterator {
            current: None,
            next: None,
            right_branches: vec![],
        };
        iter.iterate_expr(expr.clone());
        iter
    }

    fn iterate_expr(&mut self, expr: Expr) {
        self.current = Some(expr.clone());
        match expr {
            Expr::Seq(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            Expr::Choice(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            Expr::PosPred(expr)
            | Expr::NegPred(expr)
            | Expr::Rep(expr)
            | Expr::RepOnce(expr)
            | Expr::RepExact(expr, _)
            | Expr::RepMin(expr, _)
            | Expr::RepMax(expr, _)
            | Expr::RepMinMax(expr, ..)
            | Expr::Opt(expr)
            | Expr::Push(expr) => {
                self.next = Some(*expr);
            }
            #[cfg(feature = "grammar-extras")]
            Expr::NodeTag(expr, _) => {
                self.next = Some(*expr);
            }
            _ => {
                self.next = None;
            }
        }
    }
}

impl Iterator for ExprTopDownIterator {
    type Item = Expr;

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
    fn top_down_iterator() {
        let expr = Expr::Choice(
            Box::new(Expr::Str(String::from("a"))),
            Box::new(Expr::Str(String::from("b"))),
        );
        let mut top_down = expr.iter_top_down();
        assert_eq!(top_down.next(), Some(expr));
        assert_eq!(top_down.next(), Some(Expr::Str(String::from("a"))));
        assert_eq!(top_down.next(), Some(Expr::Str(String::from("b"))));
        assert_eq!(top_down.next(), None);
    }

    #[test]
    fn identity() {
        let expr = Expr::Choice(
            Box::new(Expr::Seq(
                Box::new(Expr::Ident("a".to_owned())),
                Box::new(Expr::Str("b".to_owned())),
            )),
            Box::new(Expr::PosPred(Box::new(Expr::NegPred(Box::new(Expr::Rep(
                Box::new(Expr::RepOnce(Box::new(Expr::Opt(Box::new(Expr::Choice(
                    Box::new(Expr::Insens("c".to_owned())),
                    Box::new(Expr::Push(Box::new(Expr::Range(
                        "'d'".to_owned(),
                        "'e'".to_owned(),
                    )))),
                )))))),
            )))))),
        );

        assert_eq!(
            expr.clone()
                .map_bottom_up(|expr| expr)
                .map_top_down(|expr| expr),
            expr
        );
    }
}
