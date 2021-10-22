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

use parser::{ParserExpr, ParserNode, ParserRule, Rule};
use UNICODE_PROPERTY_NAMES;

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

/// Lexically validates the grammar rules: Checks that rule identifiers are
/// unique and not a rust or pest keyword, and checks that each referenced rule
/// identifier in a rule definition is either a pest built-in or an existing
/// rule identifier.
///
/// Returns a list of the built-ins used, or the resulting errors.
///
/// Because pest grammars allow forward references in rule expressions,
/// validation requires two full passes over the provided rules: The first
/// analyzes each rule identifier (`a` in `a = { b ~ c }`), and the second
/// analyzes each referenced identifier in the rule expressions (`b` and `c` in
/// `a = { b ~ c }`).
#[allow(clippy::needless_pass_by_value)]
pub fn validate_pairs(grammar_rule_pairs: Pairs<Rule>) -> Result<Vec<&str>, Vec<Error<Rule>>> {
    let mut errors = Vec::new();
    let mut rule_identifiers = HashSet::new();
    let mut referenced_rules = HashSet::new();

    // First loop checks rule identifiers
    for rule_pair in grammar_rule_pairs.filter(|pair| pair.as_rule() == Rule::grammar_rule) {
        let span = rule_pair.clone().into_inner().next().unwrap().as_span();
        let name = span.as_str();

        // If an identifier is both defined multiple times and a rust or pest keyword, only show
        // the "already defined" error, as the first instance of the identifier will show the
        // rust/pest keyword error.
        if rule_identifiers.contains(&name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("rule {} already defined", name),
                },
                span.clone(),
            ))
        } else {
            rule_identifiers.insert(name);

            if RUST_KEYWORDS.contains(name) {
                errors.push(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("{} is a rust keyword", name),
                    },
                    span.clone(),
                ))
            } else if PEST_KEYWORDS.contains(name) {
                errors.push(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("{} is a pest keyword", name),
                    },
                    span.clone(),
                ))
            }
        }

        // Keep track of all rules that are references inside of definitions
        referenced_rules.extend(
            rule_pair
                .into_inner()
                .flatten()
                .skip(1)
                .filter(|pair| pair.as_rule() == Rule::identifier)
                .map(|pair| pair.as_span()),
        );
    }

    let mut used_builtins = Vec::new();

    // Second loop checks for referenced rule identifiers
    for span in referenced_rules {
        let name = span.as_str();

        if BUILTINS.contains(name) {
            used_builtins.push(name);
        } else if !rule_identifiers.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("rule {} is undefined", name),
                },
                span.clone(),
            ))
        }
    }

    if errors.is_empty() {
        Ok(used_builtins)
    } else {
        Err(errors)
    }
}

#[allow(clippy::ptr_arg)]
pub fn validate_ast<'a, 'i: 'a>(rules: &'a Vec<ParserRule<'i>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];
    let map: HashMap<String, &ParserNode> =
        rules.iter().map(|r| (r.name.clone(), &r.node)).collect();

    for rule in rules {
        errors.extend(validate_repetition(&rule, &map));
        errors.extend(validate_choices(&rule, &map));

        if let Some(error) = validate_whitespace_comment(&rule, &map) {
            errors.push(error);
        }

        if let Some(error) = validate_left_recursion(&rule, &map) {
            errors.push(error);
        }
    }

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
    match expr {
        ParserExpr::Str(string) => string.is_empty(),
        ParserExpr::Ident(ident) => {
            if ident == "SOI" || ident == "EOI" {
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
        ParserExpr::Seq(lhs, rhs) => {
            is_non_progressing(&lhs.expr, rules, trace)
                && is_non_progressing(&rhs.expr, rules, trace)
        }
        ParserExpr::Choice(lhs, rhs) => {
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

fn validate_repetition<'a, 'i: 'a>(
    rule: &'a ParserRule<'i>,
    map: &HashMap<String, &'a ParserNode<'i>>,
) -> Vec<Error<Rule>> {
    rule.node
        .clone()
        .filter_map_top_down(|node| match node.expr {
            ParserExpr::Rep(ref other)
            | ParserExpr::RepOnce(ref other)
            | ParserExpr::RepMin(ref other, _) => {
                if is_non_failing(&other.expr, &map, &mut vec![]) {
                    Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "expression inside repetition cannot fail and will repeat \
                                     infinitely"
                                .to_owned(),
                        },
                        node.span.clone(),
                    ))
                } else if is_non_progressing(&other.expr, &map, &mut vec![]) {
                    Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message:
                                "expression inside repetition is non-progressing and will repeat \
                                     infinitely"
                                    .to_owned(),
                        },
                        node.span.clone(),
                    ))
                } else {
                    None
                }
            }
            _ => None,
        })
}

fn validate_choices<'a, 'i: 'a>(
    rule: &'a ParserRule<'i>,
    map: &HashMap<String, &'a ParserNode<'i>>,
) -> Vec<Error<Rule>> {
    rule.node
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
                            message: "expression cannot fail; following choices cannot be reached"
                                .to_owned(),
                        },
                        node.span.clone(),
                    ))
                } else {
                    None
                }
            }
            _ => None,
        })
}

fn validate_whitespace_comment<'a, 'i: 'a>(
    rule: &'a ParserRule<'i>,
    map: &HashMap<String, &'a ParserNode<'i>>,
) -> Option<Error<Rule>> {
    match rule.name.as_str() {
        "WHITESPACE" | "COMMENT" => {
            if is_non_failing(&rule.node.expr, &map, &mut vec![]) {
                Some(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("{} cannot fail and will repeat infinitely", &rule.name),
                    },
                    rule.node.span.clone(),
                ))
            } else if is_non_progressing(&rule.node.expr, &map, &mut vec![]) {
                Some(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!(
                            "{} is non-progressing and will repeat infinitely",
                            &rule.name
                        ),
                    },
                    rule.node.span.clone(),
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

#[allow(clippy::needless_pass_by_value)]
fn validate_left_recursion<'a, 'i: 'a>(
    rule: &'a ParserRule<'i>,
    map: &HashMap<String, &'a ParserNode<'i>>,
) -> Option<Error<Rule>> {
    let name = rule.name.clone();
    let node = rule.node.clone();

    check_expr(&node, &map, &mut vec![name])
}

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
                        ),
                    },
                    node.span.clone(),
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
            check_expr(&lhs, rules, trace).or_else(|| check_expr(&rhs, rules, trace))
        }
        ParserExpr::Rep(ref node) => check_expr(&node, rules, trace),
        ParserExpr::RepOnce(ref node) => check_expr(&node, rules, trace),
        ParserExpr::Opt(ref node) => check_expr(&node, rules, trace),
        ParserExpr::PosPred(ref node) => check_expr(&node, rules, trace),
        ParserExpr::NegPred(ref node) => check_expr(&node, rules, trace),
        ParserExpr::Push(ref node) => check_expr(&node, rules, trace),
        _ => None,
    }
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

 --> 1:1
  |
1 | let = { \"a\" } let = { \"a\" }
  | ^-^
  |
  = let is a rust keyword

 --> 1:15
  |
1 | let = { \"a\" } let = { \"a\" }
  |               ^-^
  |
  = rule let already defined")]
    fn already_defined_with_keyword() {
        let input = "let = { \"a\" } let = { \"a\" }";
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
1 | COMMENT = { SOI }
  |             ^-^
  |
  = COMMENT is non-progressing and will repeat infinitely")]
    fn non_progressing_comment() {
        let input = "COMMENT = { SOI }";
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
1 | a = { (\"\" ~ &\"a\" ~ !\"a\" ~ (SOI | EOI))* }
  |       ^-------------------------------^
  |
  = expression inside repetition is non-progressing and will repeat infinitely")]
    fn non_progressing_repetition() {
        let input = "a = { (\"\" ~ &\"a\" ~ !\"a\" ~ (SOI | EOI))* }";
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
