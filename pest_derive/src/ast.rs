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
    pub ty:   RuleType,
    pub body: Body
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleType {
    Normal,
    Silent,
    Atomic,
    NonAtomic
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Body {
    Normal(Expr),
    Infix(Expr, Vec<(Rule, bool)>)
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

pub fn map_rules<F>(rules: Vec<Rule>, mut f: F) -> Vec<Rule> where F: FnMut(Rule) -> Rule {
    rules.into_iter().map(move |rule| {
        match rule {
            Rule { body: Body::Normal(_), .. } => f(rule),
            Rule { name, ty, body: Body::Infix(expr, rules) } => {
                let rule = Rule {
                    name: name,
                    ty:   ty,
                    body: Body::Infix(expr, rules.into_iter().map(|pair| {
                        let (rule, assoc) = pair;

                        (f(rule), assoc)
                    }).collect())
                };

                f(rule)
            }
        }
    }).collect()
}

pub fn map_expr<F>(expr: Expr, f: &mut F) -> Expr where F: FnMut(Expr) -> Expr {
    match expr {
        Expr::Char(_)       => f(expr),
        Expr::Str(_)        => f(expr),
        Expr::Insens(_)     => f(expr),
        Expr::Range(..)     => f(expr),
        Expr::Ident(_)      => f(expr),
        Expr::Seq(exprs)    => {
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
        Expr::RepOne(expr)  => {
            let mapped = Box::new(f(*expr));
            f(Expr::RepOne(mapped))
        },
        Expr::Opt(expr)     => {
            let mapped = Box::new(f(*expr));
            f(Expr::Opt(mapped))
        },
        Expr::PosLhd(expr)  => {
            let mapped = Box::new(f(*expr));
            f(Expr::PosLhd(mapped))
        },
        Expr::NegLhd(expr)  => {
            let mapped = Box::new(f(*expr));
            f(Expr::NegLhd(mapped))
        },
        Expr::Push(expr)    => {
            let mapped = Box::new(f(*expr));
            f(Expr::Push(mapped))
        }
    }
}

pub fn map_all_exprs<F>(rules: Vec<Rule>, mut f: F) -> Vec<Rule> where F: FnMut(Expr) -> Expr {
    fn map_rec<F>(rules: Vec<Rule>, f: &mut F) -> Vec<Rule> where F: FnMut(Expr) -> Expr {
        rules.into_iter().map(move |rule| {
            match rule {
                Rule { name, ty, body: Body::Normal(expr) } => {
                    Rule {
                        name: name,
                        ty:   ty,
                        body: Body::Normal(map_expr(expr, f))
                    }
                }
                Rule { name, ty, body: Body::Infix(expr, rules) } => {
                    let assocs: Vec<bool> = rules.iter().map(|pair| pair.1).collect();
                    let rules = map_rec(rules.into_iter().map(|pair| pair.0).collect(), f);

                    Rule {
                        name: name,
                        ty:   ty,
                        body: Body::Infix(f(expr), rules.into_iter().zip(assocs).collect())
                    }
                }
            }
        }).collect()
    }

    map_rec(rules, &mut f)
}
