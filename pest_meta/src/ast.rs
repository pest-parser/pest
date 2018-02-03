// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule<'i> {
    pub name: &'i str,
    pub ty: RuleType,
    pub expr: Expr<'i>
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    Normal,
    Silent,
    Atomic,
    CompoundAtomic,
    NonAtomic
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<'i> {
    Str(&'i str),
    Insens(&'i str),
    Range(&'i str, &'i str),
    Ident(&'i str),
    PosPred(Box<Expr<'i>>),
    NegPred(Box<Expr<'i>>),
    Seq(Box<Expr<'i>>, Box<Expr<'i>>),
    Choice(Box<Expr<'i>>, Box<Expr<'i>>),
    Opt(Box<Expr<'i>>),
    Rep(Box<Expr<'i>>),
    RepOnce(Box<Expr<'i>>),
    RepExact(Box<Expr<'i>>, u32),
    RepMin(Box<Expr<'i>>, u32),
    RepMax(Box<Expr<'i>>, u32),
    RepMinMax(Box<Expr<'i>>, u32, u32),
    Push(Box<Expr<'i>>)
}

impl<'i> Expr<'i> {
    pub fn map_top_down<F>(self, mut f: F) -> Expr<'i>
    where
        F: FnMut(Expr<'i>) -> Expr<'i>
    {
        pub fn map_internal<'i, F>(expr: Expr<'i>, f: &mut F) -> Expr<'i>
        where
            F: FnMut(Expr<'i>) -> Expr<'i>
        {
            let expr = f(expr);

            match expr {
                // TODO: Use box syntax when it gets stabilized.
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
                expr => expr
            }
        }

        map_internal(self, &mut f)
    }

    pub fn map_bottom_up<F>(self, mut f: F) -> Expr<'i>
    where
        F: FnMut(Expr<'i>) -> Expr<'i>
    {
        pub fn map_internal<'i, F>(expr: Expr<'i>, f: &mut F) -> Expr<'i>
        where
            F: FnMut(Expr<'i>) -> Expr<'i>
        {
            let mapped = match expr {
                Expr::PosPred(expr) => {
                    // TODO: Use box syntax when it gets stabilized.
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
                expr => expr
            };

            f(mapped)
        }

        map_internal(self, &mut f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity() {
        let expr = Expr::Choice(
            Box::new(Expr::Seq(
                Box::new(Expr::Ident("a")),
                Box::new(Expr::Str(&"b"))
            )),
            Box::new(Expr::PosPred(Box::new(Expr::NegPred(Box::new(
                Expr::Rep(Box::new(Expr::RepOnce(Box::new(Expr::Opt(Box::new(
                    Expr::Choice(
                        Box::new(Expr::Insens(&"c")),
                        Box::new(Expr::Push(Box::new(Expr::Range(&"'d'", &"'e'"))))
                    )
                ))))))
            )))))
        );

        assert_eq!(
            expr.clone()
                .map_bottom_up(|expr| expr)
                .map_top_down(|expr| expr),
            expr
        );
    }
}
