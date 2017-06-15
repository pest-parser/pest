// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::{HashMap, HashSet};

use pest::Error;
use pest::inputs::{Input, Span};
use pest::iterators::Pairs;

use quote::Ident;

use super::ast::*;
use super::parser::GrammarRule;

pub fn validate_pairs<I: Input>(pairs: Pairs<GrammarRule, I>) {
    let mut rust_keywords = HashSet::new();
    rust_keywords.insert("abstract");
    rust_keywords.insert("alignof");
    rust_keywords.insert("as");
    rust_keywords.insert("become");
    rust_keywords.insert("box");
    rust_keywords.insert("break");
    rust_keywords.insert("const");
    rust_keywords.insert("continue");
    rust_keywords.insert("crate");
    rust_keywords.insert("do");
    rust_keywords.insert("else");
    rust_keywords.insert("enum");
    rust_keywords.insert("extern");
    rust_keywords.insert("false");
    rust_keywords.insert("final");
    rust_keywords.insert("fn");
    rust_keywords.insert("for");
    rust_keywords.insert("if");
    rust_keywords.insert("impl");
    rust_keywords.insert("in");
    rust_keywords.insert("let");
    rust_keywords.insert("loop");
    rust_keywords.insert("macro");
    rust_keywords.insert("match");
    rust_keywords.insert("mod");
    rust_keywords.insert("move");
    rust_keywords.insert("mut");
    rust_keywords.insert("offsetof");
    rust_keywords.insert("override");
    rust_keywords.insert("priv");
    rust_keywords.insert("proc");
    rust_keywords.insert("pure");
    rust_keywords.insert("pub");
    rust_keywords.insert("ref");
    rust_keywords.insert("return");
    rust_keywords.insert("Self");
    rust_keywords.insert("self");
    rust_keywords.insert("sizeof");
    rust_keywords.insert("static");
    rust_keywords.insert("struct");
    rust_keywords.insert("super");
    rust_keywords.insert("trait");
    rust_keywords.insert("true");
    rust_keywords.insert("type");
    rust_keywords.insert("typeof");
    rust_keywords.insert("unsafe");
    rust_keywords.insert("unsized");
    rust_keywords.insert("use");
    rust_keywords.insert("virtual");
    rust_keywords.insert("where");
    rust_keywords.insert("while");
    rust_keywords.insert("yield");

    let mut pest_keywords = HashSet::new();
    pest_keywords.insert("eoi");
    pest_keywords.insert("peek");
    pest_keywords.insert("pop");
    pest_keywords.insert("push");
    pest_keywords.insert("soi");

    let definitions: Vec<_> = pairs.clone()
                                   .filter(|pair| pair.as_rule() == GrammarRule::rule)
                                   .map(|pair| {
                                       pair.into_inner()
                                           .next()
                                           .unwrap()
                                           .into_span()
                                   }).collect();
    let called_rules: Vec<_> = pairs.clone()
                                    .filter(|pair| pair.as_rule() == GrammarRule::rule)
                                    .flat_map(|pair| {
                                        let expr = pair.into_inner()
                                                       .skip(4)
                                                       .next()
                                                       .unwrap()
                                                       .into_inner();

                                        expr.flatten().filter(|pair| {
                                            pair.as_rule() == GrammarRule::identifier
                                        }).map(|pair| {
                                            pair.into_span()
                                        })
                                    }).collect();

    let mut errors = vec![];

    errors.extend(validate_rust_keywords(&definitions, &rust_keywords));
    errors.extend(validate_pest_keywords(&definitions, &pest_keywords));
    errors.extend(validate_already_defined(&definitions));
    // TODO: Add the actual set of predefined rules.
    errors.extend(validate_undefined(&definitions, &called_rules, &HashSet::new()));

    let errors = errors.into_iter().map(|error| {
        format!("grammar error\n\n{}", error)
    }).collect::<Vec<_>>().join("\n\n");

    if errors.len() > 0 {
        panic!("{}", errors);
    }
}

pub fn validate_rust_keywords<I: Input>(
    definitions: &Vec<Span<I>>,
    rust_keywords: &HashSet<&str>
) -> Vec<Error<GrammarRule, I>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.capture();

        if rust_keywords.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("{} is a rust keyword", name),
                span: definition.clone()
            })
        }
    }

    errors
}

pub fn validate_pest_keywords<I: Input>(
    definitions: &Vec<Span<I>>,
    pest_keywords: &HashSet<&str>
) -> Vec<Error<GrammarRule, I>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.capture();

        if pest_keywords.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("{} is a pest keyword", name),
                span: definition.clone()
            })
        }
    }

    errors
}

pub fn validate_already_defined<I: Input>(
    definitions: &Vec<Span<I>>
) -> Vec<Error<GrammarRule, I>> {
    let mut errors = vec![];
    let mut defined = HashSet::new();

    for definition in definitions {
        let name = definition.capture();

        if defined.contains(&name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("rule {} already defined", name),
                span: definition.clone()
            })
        } else {
            defined.insert(name);
        }
    }

    errors
}

pub fn validate_undefined<I: Input>(
    definitions: &Vec<Span<I>>,
    called_rules: &Vec<Span<I>>,
    predefined: &HashSet<&str>
) -> Vec<Error<GrammarRule, I>> {
    let mut errors = vec![];
    let definitions: HashSet<_> = definitions.iter().map(|span| span.capture()).collect();

    for rule in called_rules {
        let name = rule.capture();

        if !definitions.contains(name) && !predefined.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("rule {} is undefined", name),
                span: rule.clone()
            })
        }
    }

    errors
}

pub fn validate_ast<I: Input>(rules: &Vec<(Rule, Span<I>)>) {
    let mut errors = vec![];

    errors.extend(validate_left_recursion(rules));

    errors.sort_by_key(|error| {
        match *error {
            Error::CustomErrorSpan { ref span, .. } => span.clone(),
            _ => unreachable!()
        }
    });

    let errors = errors.into_iter().map(|error| {
        format!("{}", error)
    }).collect::<Vec<_>>().join("\n\n");

    if errors.len() > 0 {
        panic!("grammar error\n\n{}", errors);
    }
}

fn validate_left_recursion<I: Input>(rules: &Vec<(Rule, Span<I>)>) -> Vec<Error<GrammarRule, I>> {
    left_recursion(to_hash_map(rules))
}

fn to_hash_map<I: Input>(rules: &Vec<(Rule, Span<I>)>) -> HashMap<Ident, (&Expr, &Span<I>)> {
    let mut hash_map = HashMap::new();

    for &(ref rule, ref span) in rules {
        hash_map.insert(rule.name.clone(), (&rule.expr, span));
    }

    hash_map
}

fn left_recursion<I: Input>(rules: HashMap<Ident, (&Expr, &Span<I>)>) -> Vec<Error<GrammarRule, I>> {
    fn check_expr<I: Input>(
        mut names: HashSet<Ident>,
        expr: &Expr,
        span: &Span<I>,
        rules: &HashMap<Ident, (&Expr, &Span<I>)>
    ) -> Option<Error<GrammarRule, I>> {
        match *expr {
            Expr::Ident(ref other)  => {
                if names.contains(other) {
                    return Some(Error::CustomErrorSpan {
                        message: format!("rule {} is left-recursive", span.capture()),
                        span: span.clone()
                    });
                }

                names.insert(other.clone());

                check_expr(names, rules.get(other).unwrap().0, span, rules)
            },
            Expr::Seq(ref lhs, _) => check_expr(names, &lhs, span, rules),
            Expr::Choice(ref lhs, _) => check_expr(names, &lhs, span, rules),
            Expr::Rep(ref expr) => check_expr(names, &expr, span, rules),
            Expr::RepOnce(ref expr) => check_expr(names, &expr, span, rules),
            Expr::Opt(ref expr) => check_expr(names, &expr, span, rules),
            Expr::PosPred(ref expr) => check_expr(names, &expr, span, rules),
            Expr::NegPred(ref expr) => check_expr(names, &expr, span, rules),
            Expr::Push(ref expr) => check_expr(names, &expr, span, rules),
            _ => None
        }
    }

    let mut errors = vec![];

    for (ref name, &(ref expr, ref span)) in &rules {
        let mut names = HashSet::new();

        names.insert((*name).clone());

        if let Some(error) = check_expr(names, expr, span, &rules) {
            errors.push(error);
        }
    }

    errors
}
