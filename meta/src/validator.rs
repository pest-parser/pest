// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};

use pest::error::{Error, ErrorVariant, InputLocation};
use pest::iterators::Pairs;
use pest::Span;

use crate::parser::{ParserExpr, ParserNode, ParserRule, Rule};
use crate::UNICODE_PROPERTY_NAMES;

static RUST_KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        "abstract", "alignof", "as", "become", "box", "break", "const", "continue", "crate", "do",
        "else", "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in", "let", "loop",
        "macro", "match", "mod", "move", "mut", "offsetof", "override", "priv", "proc", "pure",
        "pub", "ref", "return", "Self", "self", "sizeof", "static", "struct", "super", "trait",
        "true", "type", "typeof", "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
    ]
    .iter()
    .cloned()
    .collect()
});

static PEST_KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        "_", "ANY", "DROP", "EOI", "PEEK", "PEEK_ALL", "POP", "POP_ALL", "PUSH", "SOI",
    ]
    .iter()
    .cloned()
    .collect()
});

static BUILTINS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        "ANY",
        "DROP",
        "EOI",
        "PEEK",
        "PEEK_ALL",
        "POP",
        "POP_ALL",
        "SOI",
        "ASCII_DIGIT",
        "ASCII_NONZERO_DIGIT",
        "ASCII_BIN_DIGIT",
        "ASCII_OCT_DIGIT",
        "ASCII_HEX_DIGIT",
        "ASCII_ALPHA_LOWER",
        "ASCII_ALPHA_UPPER",
        "ASCII_ALPHA",
        "ASCII_ALPHANUMERIC",
        "ASCII",
        "NEWLINE",
    ]
    .iter()
    .cloned()
    .chain(UNICODE_PROPERTY_NAMES.iter().cloned())
    .collect::<HashSet<&str>>()
});

pub fn validate_pairs(pairs: Pairs<Rule>) -> Result<Vec<&str>, Vec<Error<Rule>>> {
    let definitions: Vec<_> = pairs
        .clone()
        .filter(|pair| pair.as_rule() == Rule::grammar_rule)
        .map(|pair| pair.into_inner().next().unwrap().as_span())
        .collect();
    let called_rules: Vec<_> = pairs
        .clone()
        .filter(|pair| pair.as_rule() == Rule::grammar_rule)
        .flat_map(|pair| {
            pair.into_inner()
                .flatten()
                .skip(1)
                .filter(|pair| pair.as_rule() == Rule::identifier)
                .map(|pair| pair.as_span())
        })
        .collect();

    let mut errors = vec![];

    errors.extend(validate_rust_keywords(&definitions));
    errors.extend(validate_pest_keywords(&definitions));
    errors.extend(validate_already_defined(&definitions));
    errors.extend(validate_undefined(&definitions, &called_rules));

    if !errors.is_empty() {
        return Err(errors);
    }

    let definitions: HashSet<_> = definitions.iter().map(|span| span.as_str()).collect();
    let called_rules: HashSet<_> = called_rules.iter().map(|span| span.as_str()).collect();

    let defaults = called_rules.difference(&definitions);

    Ok(defaults.cloned().collect())
}

#[allow(clippy::ptr_arg)]
pub fn validate_rust_keywords(definitions: &Vec<Span>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if RUST_KEYWORDS.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("{} is a rust keyword", name),
                },
                *definition,
            ))
        }
    }

    errors
}

#[allow(clippy::ptr_arg)]
pub fn validate_pest_keywords(definitions: &Vec<Span>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if PEST_KEYWORDS.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("{} is a pest keyword", name),
                },
                *definition,
            ))
        }
    }

    errors
}

#[allow(clippy::ptr_arg)]
pub fn validate_already_defined(definitions: &Vec<Span>) -> Vec<Error<Rule>> {
    let mut errors = vec![];
    let mut defined = HashSet::new();

    for definition in definitions {
        let name = definition.as_str();

        if defined.contains(&name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("rule {} already defined", name),
                },
                *definition,
            ))
        } else {
            defined.insert(name);
        }
    }

    errors
}

#[allow(clippy::ptr_arg)]
pub fn validate_undefined<'i>(
    definitions: &Vec<Span<'i>>,
    called_rules: &Vec<Span<'i>>,
) -> Vec<Error<Rule>> {
    let mut errors = vec![];
    let definitions: HashSet<_> = definitions.iter().map(|span| span.as_str()).collect();

    for rule in called_rules {
        let name = rule.as_str();

        if !definitions.contains(name) && !BUILTINS.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("rule {} is undefined", name),
                },
                *rule,
            ))
        }
    }

    errors
}

#[allow(clippy::ptr_arg)]
pub fn validate_ast<'a, 'i: 'a>(rules: &'a Vec<ParserRule<'i>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    errors.extend(validate_repetition(rules));
    errors.extend(validate_choices(rules));
    errors.extend(validate_whitespace_comment(rules));
    errors.extend(validate_left_recursion(rules));

    errors.sort_by_key(|error| match error.location {
        InputLocation::Span(span) => span,
        _ => unreachable!(),
    });

    errors
}

fn is_non_progressing<'i>(
    expr: &ParserExpr<'i>,
    rules: &HashMap<String, &ParserNode<'i>>,
    trace: &mut Vec<String>,
) -> bool {
    match *expr {
        ParserExpr::Str(ref string) => string.is_empty(),
        ParserExpr::Ident(ref ident) => {
            if ident == "soi" || ident == "eoi" {
                return true;
            }

            if !trace.contains(ident) {
                if let Some(node) = rules.get(ident) {
                    trace.push(ident.clone());
                    let result = is_non_progressing(&node.expr, rules, trace);
                    trace.pop().unwrap();

                    return result;
                }
            }

            false
        }
        ParserExpr::PosPred(_) => true,
        ParserExpr::NegPred(_) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr, rules, trace)
                && is_non_progressing(&rhs.expr, rules, trace)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr, rules, trace)
                || is_non_progressing(&rhs.expr, rules, trace)
        }
        _ => false,
    }
}

fn is_non_failing<'i>(
    expr: &ParserExpr<'i>,
    rules: &HashMap<String, &ParserNode<'i>>,
    trace: &mut Vec<String>,
) -> bool {
    match *expr {
        ParserExpr::Str(ref string) => string.is_empty(),
        ParserExpr::Ident(ref ident) => {
            if !trace.contains(ident) {
                if let Some(node) = rules.get(ident) {
                    trace.push(ident.clone());
                    let result = is_non_failing(&node.expr, rules, trace);
                    trace.pop().unwrap();

                    return result;
                }
            }

            false
        }
        ParserExpr::Opt(_) => true,
        ParserExpr::Rep(_) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr, rules, trace) && is_non_failing(&rhs.expr, rules, trace)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr, rules, trace) || is_non_failing(&rhs.expr, rules, trace)
        }
        _ => false,
    }
}

fn validate_repetition<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    let mut result = vec![];
    let map = to_hash_map(rules);

    for rule in rules {
        let mut errors = rule.node
            .clone()
            .filter_map_top_down(|node| match node.expr {
                ParserExpr::Rep(ref other)
                | ParserExpr::RepOnce(ref other)
                | ParserExpr::RepMin(ref other, _) => {
                    if is_non_failing(&other.expr, &map, &mut vec![]) {
                        Some(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message:
                                    "expression inside repetition cannot fail and will repeat \
                                     infinitely"
                                        .to_owned()
                            },
                            node.span
                        ))
                    } else if is_non_progressing(&other.expr, &map, &mut vec![]) {
                        Some(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message:
                                    "expression inside repetition is non-progressing and will repeat \
                                     infinitely"
                                        .to_owned(),
                            },
                            node.span
                        ))
                    } else {
                        None
                    }
                }
                _ => None
            });

        result.append(&mut errors);
    }

    result
}

fn validate_choices<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    let mut result = vec![];
    let map = to_hash_map(rules);

    for rule in rules {
        let mut errors = rule
            .node
            .clone()
            .filter_map_top_down(|node| match node.expr {
                ParserExpr::Choice(ref lhs, _) => {
                    let node = match lhs.expr {
                        ParserExpr::Choice(_, ref rhs) => rhs,
                        _ => lhs,
                    };

                    if is_non_failing(&node.expr, &map, &mut vec![]) {
                        Some(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message:
                                    "expression cannot fail; following choices cannot be reached"
                                        .to_owned(),
                            },
                            node.span,
                        ))
                    } else {
                        None
                    }
                }
                _ => None,
            });

        result.append(&mut errors);
    }

    result
}

fn validate_whitespace_comment<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    let map = to_hash_map(rules);

    rules
        .iter()
        .filter_map(|rule| {
            if rule.name == "WHITESPACE" || rule.name == "COMMENT" {
                if is_non_failing(&rule.node.expr, &map, &mut vec![]) {
                    Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "{} cannot fail and will repeat infinitely",
                                &rule.name
                            ),
                        },
                        rule.node.span,
                    ))
                } else if is_non_progressing(&rule.node.expr, &map, &mut vec![]) {
                    Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "{} is non-progressing and will repeat infinitely",
                                &rule.name
                            ),
                        },
                        rule.node.span,
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect()
}

fn validate_left_recursion<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    left_recursion(to_hash_map(rules))
}

fn to_hash_map<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> HashMap<String, &'a ParserNode<'i>> {
    rules.iter().map(|r| (r.name.clone(), &r.node)).collect()
}

fn left_recursion<'a, 'i: 'a>(rules: HashMap<String, &'a ParserNode<'i>>) -> Vec<Error<Rule>> {
    fn check_expr<'a, 'i: 'a>(
        node: &'a ParserNode<'i>,
        rules: &'a HashMap<String, &ParserNode<'i>>,
        trace: &mut Vec<String>,
    ) -> Option<Error<Rule>> {
        match node.expr.clone() {
            ParserExpr::Ident(other) => {
                if trace[0] == other {
                    trace.push(other);
                    let chain = trace
                        .iter()
                        .map(|ident| ident.as_ref())
                        .collect::<Vec<_>>()
                        .join(" -> ");

                    return Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "rule {} is left-recursive ({}); pest::prec_climber might be useful \
                                 in this case",
                                node.span.as_str(),
                                chain
                            )
                        },
                        node.span
                    ));
                }

                if !trace.contains(&other) {
                    if let Some(node) = rules.get(&other) {
                        trace.push(other);
                        let result = check_expr(node, rules, trace);
                        trace.pop().unwrap();

                        return result;
                    }
                }

                None
            }
            ParserExpr::Seq(ref lhs, ref rhs) => {
                if is_non_failing(&lhs.expr, rules, &mut vec![trace.last().unwrap().clone()]) {
                    check_expr(rhs, rules, trace)
                } else {
                    check_expr(lhs, rules, trace)
                }
            }
            ParserExpr::Choice(ref lhs, ref rhs) => {
                check_expr(lhs, rules, trace).or_else(|| check_expr(rhs, rules, trace))
            }
            ParserExpr::Rep(ref node) => check_expr(node, rules, trace),
            ParserExpr::RepOnce(ref node) => check_expr(node, rules, trace),
            ParserExpr::Opt(ref node) => check_expr(node, rules, trace),
            ParserExpr::PosPred(ref node) => check_expr(node, rules, trace),
            ParserExpr::NegPred(ref node) => check_expr(node, rules, trace),
            ParserExpr::Push(ref node) => check_expr(node, rules, trace),
            _ => None,
        }
    }

    let mut errors = vec![];

    for (name, node) in &rules {
        let name = name.clone();

        if let Some(error) = check_expr(node, &rules, &mut vec![name]) {
            errors.push(error);
        }
    }

    errors
}

#[cfg(test)]
mod tests {
    use super::super::parser::{consume_rules, PestParser};
    use super::super::unwrap_or_report;
    use super::*;
    use pest::Parser;

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:1
  |
1 | let = { \"a\" }
  | ^-^
  |
  = let is a rust keyword")]
    fn rust_keyword() {
        let input = "let = { \"a\" }";
        unwrap_or_report(validate_pairs(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:1
  |
1 | ANY = { \"a\" }
  | ^-^
  |
  = ANY is a pest keyword")]
    fn pest_keyword() {
        let input = "ANY = { \"a\" }";
        unwrap_or_report(validate_pairs(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:13
  |
1 | a = { \"a\" } a = { \"a\" }
  |             ^
  |
  = rule a already defined")]
    fn already_defined() {
        let input = "a = { \"a\" } a = { \"a\" }";
        unwrap_or_report(validate_pairs(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { b }
  |       ^
  |
  = rule b is undefined")]
    fn undefined() {
        let input = "a = { b }";
        unwrap_or_report(validate_pairs(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    fn valid_recursion() {
        let input = "a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"b\") ~ a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:16
  |
1 | WHITESPACE = { \"\" }
  |                ^^
  |
  = WHITESPACE cannot fail and will repeat infinitely")]
    fn non_failing_whitespace() {
        let input = "WHITESPACE = { \"\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:13
  |
1 | COMMENT = { soi }
  |             ^-^
  |
  = COMMENT is non-progressing and will repeat infinitely")]
    fn non_progressing_comment() {
        let input = "COMMENT = { soi }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { (\"\")* }
  |       ^---^
  |
  = expression inside repetition cannot fail and will repeat infinitely")]
    fn non_failing_repetition() {
        let input = "a = { (\"\")* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:18
  |
1 | a = { \"\" } b = { a* }
  |                  ^^
  |
  = expression inside repetition cannot fail and will repeat infinitely")]
    fn indirect_non_failing_repetition() {
        let input = "a = { \"\" } b = { a* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:20
  |
1 | a = { \"a\" ~ (\"b\" ~ (\"\")*) }
  |                    ^---^
  |
  = expression inside repetition cannot fail and will repeat infinitely")]
    fn deep_non_failing_repetition() {
        let input = "a = { \"a\" ~ (\"b\" ~ (\"\")*) }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { (\"\" ~ &\"a\" ~ !\"a\" ~ (soi | eoi))* }
  |       ^-------------------------------^
  |
  = expression inside repetition is non-progressing and will repeat infinitely")]
    fn non_progressing_repetition() {
        let input = "a = { (\"\" ~ &\"a\" ~ !\"a\" ~ (soi | eoi))* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:20
  |
1 | a = { !\"a\" } b = { a* }
  |                    ^^
  |
  = expression inside repetition is non-progressing and will repeat infinitely")]
    fn indirect_non_progressing_repetition() {
        let input = "a = { !\"a\" } b = { a* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { a }
  |       ^
  |
  = rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case")]
    fn simple_left_recursion() {
        let input = "a = { a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { b } b = { a }
  |       ^
  |
  = rule b is left-recursive (b -> a -> b); pest::prec_climber might be useful in this case

 --> 1:17
  |
1 | a = { b } b = { a }
  |                 ^
  |
  = rule a is left-recursive (a -> b -> a); pest::prec_climber might be useful in this case")]
    fn indirect_left_recursion() {
        let input = "a = { b } b = { a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:39
  |
1 | a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"\") ~ a }
  |                                       ^
  |
  = rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case")]
    fn non_failing_left_recursion() {
        let input = "a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"\") ~ a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:13
  |
1 | a = { \"a\" | a }
  |             ^
  |
  = rule a is left-recursive (a -> a); pest::prec_climber might be useful in this case")]
    fn non_primary_choice_left_recursion() {
        let input = "a = { \"a\" | a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { \"a\"* | \"a\" | \"b\" }
  |       ^--^
  |
  = expression cannot fail; following choices cannot be reached")]
    fn lhs_non_failing_choice() {
        let input = "a = { \"a\"* | \"a\" | \"b\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:13
  |
1 | a = { \"a\" | \"a\"* | \"b\" }
  |             ^--^
  |
  = expression cannot fail; following choices cannot be reached")]
    fn lhs_non_failing_choice_middle() {
        let input = "a = { \"a\" | \"a\"* | \"b\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { b | \"a\" } b = { \"b\"* | \"c\" }
  |       ^
  |
  = expression cannot fail; following choices cannot be reached

 --> 1:23
  |
1 | a = { b | \"a\" } b = { \"b\"* | \"c\" }
  |                       ^--^
  |
  = expression cannot fail; following choices cannot be reached")]
    fn lhs_non_failing_nested_choices() {
        let input = "a = { b | \"a\" } b = { \"b\"* | \"c\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    fn skip_can_be_defined() {
        let input = "skip = { \"\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }
}
