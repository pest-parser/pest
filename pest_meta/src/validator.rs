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

use super::parser::{PestRule, ParserExpr, ParserNode, ParserRule};

pub fn validate_pairs<I: Input>(pairs: Pairs<PestRule, I>) -> Result<Vec<Ident>, Vec<Error<PestRule, I>>> {
    let rust_keywords = hashset!{
        "abstract", "alignof", "as", "become", "box", "break", "const", "continue", "crate", "do",
        "else", "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in", "let", "loop",
        "macro", "match", "mod", "move", "mut", "offsetof", "override", "priv", "proc", "pure",
        "pub", "ref", "return", "Self", "self", "sizeof", "static", "struct", "super", "trait",
        "true", "type", "typeof", "unsafe", "unsized", "use", "virtual", "where", "while", "yield"
    };
    let pest_keywords = hashset!{"any", "eoi", "peek", "pop", "push", "soi"};
    let predefined = hashset!{"any", "eoi", "peek", "pop", "soi"};

    let definitions: Vec<_> = pairs.clone()
                                   .filter(|pair| pair.as_rule() == PestRule::grammar_rule)
                                   .map(|pair| {
                                       pair.into_inner()
                                           .next()
                                           .unwrap()
                                           .into_span()
                                   }).collect();
    let called_rules: Vec<_> = pairs.filter(|pair| pair.as_rule() == PestRule::grammar_rule)
                                    .flat_map(|pair| {
                                        pair.into_inner().flatten().skip(1).filter(|pair| {
                                            pair.as_rule() == PestRule::identifier
                                        }).map(|pair| {
                                            pair.into_span()
                                        })
                                    }).collect();

    let mut errors = vec![];

    errors.extend(validate_rust_keywords(&definitions, &rust_keywords));
    errors.extend(validate_pest_keywords(&definitions, &pest_keywords));
    errors.extend(validate_already_defined(&definitions));
    errors.extend(validate_undefined(&definitions, &called_rules, &predefined));

    if errors.len() > 0 {
        return Err(errors)
    }

    let definitions: HashSet<_> = definitions.iter().map(|span| span.as_str()).collect();
    let called_rules: HashSet<_> = called_rules.iter().map(|span| span.as_str()).collect();

    let defaults = called_rules.difference(&definitions);

    Ok(defaults.into_iter().map(|string| Ident::new(*string)).collect())
}

pub fn validate_rust_keywords<'a, I: Input+'a, Definitions: IntoIterator<Item=&'a Span<I>>>(
    definitions: Definitions,
    rust_keywords: &HashSet<&str>
) -> Vec<Error<PestRule, I>> {
    definitions.into_iter()
               .filter_map(|definition| {
                   let name = definition.as_str();

                   if rust_keywords.contains(name) {
                       Some(Error::CustomErrorSpan {
                           message: format!("{} is a rust keyword", name),
                           span: definition.clone()
                       })
                   } else {
                       None
                   }
               }).collect()
}

pub fn validate_pest_keywords<'a, I: Input+'a, Definitions: IntoIterator<Item=&'a Span<I>>>(
    definitions: Definitions,
    pest_keywords: &HashSet<&str>
) -> Vec<Error<PestRule, I>> {
    definitions.into_iter()
               .filter_map(|definition| {
                   let name = definition.as_str();

                   if pest_keywords.contains(name) {
                       Some(Error::CustomErrorSpan {
                           message: format!("{} is a pest keyword", name),
                           span: definition.clone()
                       })
                   } else {
                       None
                   }
               }).collect()
}

pub fn validate_already_defined<'a, I: Input+'a, Definitions: IntoIterator<Item=&'a Span<I>>>(
    definitions: Definitions
) -> Vec<Error<PestRule, I>> {

    let mut defined = HashSet::new();
    definitions.into_iter()
               .filter_map(|definition| {
                   let name = definition.as_str();

                   if defined.insert(name) {
                       None
                   } else {
                       Some(Error::CustomErrorSpan {
                           message: format!("rule {} already defined", name),
                           span: definition.clone()
                       })
                   }
               })
               .collect()
}

pub fn validate_undefined<I: Input>(
    definitions: &Vec<Span<I>>,
    called_rules: &Vec<Span<I>>,
    predefined: &HashSet<&str>
) -> Vec<Error<PestRule, I>> {
    let mut errors = vec![];
    let definitions: HashSet<_> = definitions.iter().map(|span| span.as_str()).collect();

    for rule in called_rules {
        let name = rule.as_str();

        if !definitions.contains(name) && !predefined.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("rule {} is undefined", name),
                span: rule.clone()
            })
        }
    }

    errors
}

pub fn validate_ast<I: Input>(rules: &Vec<ParserRule<I>>) -> Vec<Error<PestRule, I>> {
    let mut errors = vec![];

    errors.extend(validate_repetition(rules));
    errors.extend(validate_whitespace_comment(rules));
    errors.extend(validate_left_recursion(rules));

    errors.sort_by_key(|error| {
        match *error {
            Error::CustomErrorSpan { ref span, .. } => span.clone(),
            _ => unreachable!()
        }
    });

    errors
}

fn is_non_progressing<I: Input>(expr: &ParserExpr<I>) -> bool {
    match *expr {
        ParserExpr::Str(ref string) => string == "",
        ParserExpr::Ident(ref ident) => ident == "soi" || ident == "eoi",
        ParserExpr::PosPred(_) => true,
        ParserExpr::NegPred(_) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr) && is_non_progressing(&rhs.expr)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr) || is_non_progressing(&rhs.expr)
        }
        _ => false
    }
}

fn is_non_failing<I: Input>(expr: &ParserExpr<I>) -> bool {
    match *expr {
        ParserExpr::Str(ref string) => string == "",
        ParserExpr::Opt(_) => true,
        ParserExpr::Rep(_) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr) && is_non_failing(&rhs.expr)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr) || is_non_failing(&rhs.expr)
        }
        _ => false
    }
}

fn validate_repetition<I: Input>(rules: &Vec<ParserRule<I>>) -> Vec<Error<PestRule, I>> {
    rules.into_iter().filter_map(|rule| {
        match rule.node.expr {
            ParserExpr::Rep(ref other) | ParserExpr::RepOnce(ref other)=> {
                if is_non_failing(&other.expr) {
                    Some(Error::CustomErrorSpan {
                        message: "expression inside repetition is non-failing and will repeat \
                                  infinitely".to_owned(),
                        span: rule.node.span.clone()
                    })
                } else if is_non_progressing(&other.expr) {
                    Some(Error::CustomErrorSpan {
                        message: "expression inside repetition is non-progressing and will repeat \
                                  infinitely".to_owned(),
                        span: rule.node.span.clone()
                    })
                } else {
                    None
                }
            }
            _ => None
        }
    }).collect()
}

fn validate_whitespace_comment<I: Input>(rules: &Vec<ParserRule<I>>) -> Vec<Error<PestRule, I>> {
    rules.into_iter().filter_map(|rule| {
        if &rule.name == "whitespace" || &rule.name == "comment" {
            if is_non_failing(&rule.node.expr) {
                Some(Error::CustomErrorSpan {
                    message: format!("{} is non-failing and will repeat infinitely", &rule.name),
                    span: rule.node.span.clone()
                })
            } else if is_non_progressing(&rule.node.expr) {
                Some(Error::CustomErrorSpan {
                    message: format!("{} is non-progressing and will repeat infinitely",
                                     &rule.name),
                    span: rule.node.span.clone()
                })
            } else {
                None
            }
        } else {
            None
        }
    }).collect()
}

fn validate_left_recursion<I: Input>(rules: &Vec<ParserRule<I>>) -> Vec<Error<PestRule, I>> {
    left_recursion(to_hash_map(rules))
}

fn to_hash_map<I: Input>(rules: &Vec<ParserRule<I>>) -> HashMap<Ident, &ParserNode<I>> {
    let mut hash_map = HashMap::new();

    for rule in rules {
        hash_map.insert(rule.name.clone(), &rule.node);
    }

    hash_map
}

fn left_recursion<I: Input>(rules: HashMap<Ident, &ParserNode<I>>) -> Vec<Error<PestRule, I>> {
    fn check_expr<I: Input>(
        node: &ParserNode<I>,
        rules: &HashMap<Ident, &ParserNode<I>>,
        trace: &mut Vec<Ident>
    ) -> Option<Error<PestRule, I>> {
        match node.expr {
            ParserExpr::Ident(ref other)  => {
                if trace[0] == other {
                    trace.push(other.clone());
                    let chain = trace.iter()
                                     .map(|ident| ident.as_ref())
                                     .collect::<Vec<_>>()
                                     .join(" -> ");

                    return Some(Error::CustomErrorSpan {
                        message: format!("rule {} is left-recursive ({}); pest::prec_climber might \
                                          be useful in this case", node.span.as_str(), chain),
                        span: node.span.clone()
                    });
                }

                if !trace.contains(other) {
                    if let Some(node) = rules.get(other) {
                        trace.push(other.clone());
                        let result = check_expr(node, rules, trace);
                        trace.pop().unwrap();

                        return result;
                    }
                }

                None
            },
            ParserExpr::Seq(ref lhs, ref rhs) => {
                if is_non_failing(&lhs.expr) {
                    check_expr(rhs, rules, trace)
                } else {
                    check_expr(lhs, rules, trace)
                }
            }
            ParserExpr::Choice(ref lhs, ref rhs) => {
                check_expr(&lhs, rules, trace).or(check_expr(&rhs, rules, trace))
            }
            ParserExpr::Rep(ref node) => check_expr(&node, rules, trace),
            ParserExpr::RepOnce(ref node) => check_expr(&node, rules, trace),
            ParserExpr::Opt(ref node) => check_expr(&node, rules, trace),
            ParserExpr::PosPred(ref node) => check_expr(&node, rules, trace),
            ParserExpr::NegPred(ref node) => check_expr(&node, rules, trace),
            ParserExpr::Push(ref node) => check_expr(&node, rules, trace),
            _ => None
        }
    }

    let mut errors = vec![];

    for (ref name, ref node) in &rules {
        let name = (*name).clone();

        if let Some(error) = check_expr(node, &rules, &mut vec![name]) {
            errors.push(error);
        }
    }

    errors
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use super::super::parser::{PestParser, consume_rules};

    #[test]
    fn rust_keyword() {
        let input = "let = { \"a\" }";
        let err = validate_pairs(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("`let` should not be allowed because it is a rust keyword");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "let is a rust keyword");
                assert_eq!(span.start(), 0);
                assert_eq!(span.end(), 3);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn pest_keyword() {
        let input = "any = { \"a\" }";
        let err = validate_pairs(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("`any` should not be allowed because it is a pest keyword");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "any is a pest keyword");
                assert_eq!(span.start(), 0);
                assert_eq!(span.end(), 3);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn already_defined() {
        let input = "a = { \"a\" } a = { \"a\" }";
        let err = validate_pairs(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("Re-defining a rule should produce an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule a already defined");
                assert_eq!(span.start(), 12);
                assert_eq!(span.end(), 13);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn undefined() {
        let input = "a = { b }";
        let err = validate_pairs(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("Cannot use rules without defining them");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule b is undefined");
                assert_eq!(span.start(), 6);
                assert_eq!(span.end(), 7);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn valid_recursion() {
        let input = "a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"b\") ~ a }";
        consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap()).unwrap();
    }

    #[test]
    fn non_failing_whitespace() {
        let input = "whitespace = { \"\" }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "whitespace is non-failing and will repeat infinitely");
                assert_eq!(span.start(), 15);
                assert_eq!(span.end(), 17);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn non_progressing_comment() {
        let input = "comment = { soi }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "comment is non-progressing and will repeat infinitely");
                assert_eq!(span.start(), 12);
                assert_eq!(span.end(), 15);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn non_failing_repetition() {
        let input = "a = { (\"\")* }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "expression inside repetition is non-failing and will repeat infinitely");
                assert_eq!(span.start(), 6);
                assert_eq!(span.end(), 11);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn non_progressing_repetition() {
        let input = "a = { (\"\" ~ &\"a\" ~ !\"a\" ~ (soi | eoi))* }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "expression inside repetition is non-progressing and will repeat infinitely");
                assert_eq!(span.start(), 6);
                assert_eq!(span.end(), 39);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn simple_left_recursion() {
        let input = "a = { a }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case");
                assert_eq!(span.start(), 6);
                assert_eq!(span.end(), 7);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn indirect_left_recursion() {
        let input = "a = { b } b = { a }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced errors");
        assert_eq!(err.len(), 2);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule b is left-recursive (b -> a -> b); pest::prec_climber might be useful in this case");
                assert_eq!(span.start(), 6);
                assert_eq!(span.end(), 7);
            }
            ref other => panic!("Unexpected error {}", other)
        }
        match err[1] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule a is left-recursive (a -> b -> a); pest::prec_climber might be useful in this case");
                assert_eq!(span.start(), 16);
                assert_eq!(span.end(), 17);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn non_failing_left_recursion() {
        let input = "a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"\") ~ a }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case");
                assert_eq!(span.start(), 38);
                assert_eq!(span.end(), 39);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }

    #[test]
    fn non_primary_choice_left_recursion() {
        let input = "a = { \"a\" | a }";
        let err = consume_rules(PestParser::parse_str(PestRule::grammar_rules, input).unwrap())
            .expect_err("This should have produced an error");
        assert_eq!(err.len(), 1);
        match err[0] {
            Error::CustomErrorSpan{ref message, ref span} => {
                assert_eq!(message, "rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case");
                assert_eq!(span.start(), 12);
                assert_eq!(span.end(), 13);
            }
            ref other => panic!("Unexpected error {}", other)
        }
    }
}
