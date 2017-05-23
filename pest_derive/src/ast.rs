// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use quote::Ident;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    pub name: Ident,
    pub ty: RuleType,
    pub expr: Expr
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    Normal,
    Silent,
    Atomic,
    NonAtomic
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Char(String),
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(Ident),
    Seq(Vec<Expr>),
    Choice(Vec<Expr>),
    RepZero(Box<Expr>),
    RepOne(Box<Expr>),
    Opt(Box<Expr>),
    PosLhd(Box<Expr>),
    NegLhd(Box<Expr>),
    Push(Box<Expr>)
}

impl Expr {
    pub fn map<F>(self, mut f: F) -> Expr where F: FnMut(Expr) -> Expr {
        pub fn map_internal<F>(expr: Expr, f: &mut F) -> Expr where F: FnMut(Expr) -> Expr {
            match expr {
                Expr::Char(_) => f(expr),
                Expr::Str(_) => f(expr),
                Expr::Insens(_) => f(expr),
                Expr::Range(..) => f(expr),
                Expr::Ident(_) => f(expr),
                Expr::Seq(exprs) => {
                    let mapped = exprs.into_iter().map(|expr| f(expr)).collect();
                    f(Expr::Seq(mapped))
                },
                Expr::Choice(exprs) => {
                    let mapped = exprs.into_iter().map(|expr| f(expr)).collect();
                    f(Expr::Choice(mapped))
                },
                Expr::RepZero(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::RepZero(mapped))
                },
                Expr::RepOne(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::RepOne(mapped))
                },
                Expr::Opt(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::Opt(mapped))
                },
                Expr::PosLhd(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::PosLhd(mapped))
                },
                Expr::NegLhd(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::NegLhd(mapped))
                },
                Expr::Push(expr) => {
                    let mapped = Box::new(f(*expr));
                    f(Expr::Push(mapped))
                }
            }
        }

        map_internal(self, &mut f)
    }
}
