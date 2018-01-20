// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::{HashMap, HashSet};

use pest::Error;
use pest::Span;
use pest::iterators::Pairs;

use quote::Ident;

use parser::{GrammarRule, ParserExpr, ParserNode, ParserRule};

pub fn validate_pairs<'i>(pairs: Pairs<'i, GrammarRule>) -> Vec<Ident> {
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
    pest_keywords.insert("any");
    pest_keywords.insert("eoi");
    pest_keywords.insert("peek");
    pest_keywords.insert("pop");
    pest_keywords.insert("push");
    pest_keywords.insert("soi");

    let mut predefined = HashSet::new();
    predefined.insert("any");
    predefined.insert("eoi");
    predefined.insert("peek");
    predefined.insert("pop");
    predefined.insert("soi");

    let definitions: Vec<_> = pairs
        .clone()
        .filter(|pair| pair.as_rule() == GrammarRule::grammar_rule)
        .map(|pair| pair.into_inner().next().unwrap().into_span())
        .collect();
    let called_rules: Vec<_> = pairs
        .clone()
        .filter(|pair| pair.as_rule() == GrammarRule::grammar_rule)
        .flat_map(|pair| {
            pair.into_inner()
                .flatten()
                .skip(1)
                .filter(|pair| pair.as_rule() == GrammarRule::identifier)
                .map(|pair| pair.into_span())
        })
        .collect();

    let mut errors = vec![];

    errors.extend(validate_rust_keywords(&definitions, &rust_keywords));
    errors.extend(validate_pest_keywords(&definitions, &pest_keywords));
    errors.extend(validate_already_defined(&definitions));
    errors.extend(validate_undefined(&definitions, &called_rules, &predefined));

    let errors = errors
        .into_iter()
        .map(|error| format!("grammar error\n\n{}", error))
        .collect::<Vec<_>>()
        .join("\n\n");

    if errors.len() > 0 {
        panic!("{}", errors);
    }

    let definitions: HashSet<_> = definitions.iter().map(|span| span.as_str()).collect();
    let called_rules: HashSet<_> = called_rules.iter().map(|span| span.as_str()).collect();

    let defaults = called_rules.difference(&definitions);

    defaults
        .into_iter()
        .map(|string| Ident::new(*string))
        .collect()
}

pub fn validate_rust_keywords<'i>(
    definitions: &Vec<Span<'i>>,
    rust_keywords: &HashSet<&str>
) -> Vec<Error<'i, GrammarRule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if rust_keywords.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("{} is a rust keyword", name),
                span: definition.clone()
            })
        }
    }

    errors
}

pub fn validate_pest_keywords<'i>(
    definitions: &Vec<Span<'i>>,
    pest_keywords: &HashSet<&str>
) -> Vec<Error<'i, GrammarRule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if pest_keywords.contains(name) {
            errors.push(Error::CustomErrorSpan {
                message: format!("{} is a pest keyword", name),
                span: definition.clone()
            })
        }
    }

    errors
}

pub fn validate_already_defined<'i>(definitions: &Vec<Span<'i>>) -> Vec<Error<'i, GrammarRule>> {
    let mut errors = vec![];
    let mut defined = HashSet::new();

    for definition in definitions {
        let name = definition.as_str();

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

pub fn validate_undefined<'i>(
    definitions: &Vec<Span<'i>>,
    called_rules: &Vec<Span<'i>>,
    predefined: &HashSet<&str>
) -> Vec<Error<'i, GrammarRule>> {
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

pub fn validate_ast<'i>(rules: &Vec<ParserRule<'i>>) {
    let mut errors = vec![];

    errors.extend(validate_repetition(rules));
    errors.extend(validate_whitespace_comment(rules));
    errors.extend(validate_left_recursion(rules));

    errors.sort_by_key(|error| match *error {
        Error::CustomErrorSpan { ref span, .. } => span.clone(),
        _ => unreachable!()
    });

    let errors = errors
        .into_iter()
        .map(|error| format!("{}", error))
        .collect::<Vec<_>>()
        .join("\n\n");

    if errors.len() > 0 {
        panic!("grammar error\n\n{}", errors);
    }
}

fn is_non_progressing<'i>(expr: &ParserExpr<'i>) -> bool {
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

fn is_non_failing<'i>(expr: &ParserExpr<'i>) -> bool {
    match *expr {
        ParserExpr::Str(ref string) => string == "",
        ParserExpr::Opt(_) => true,
        ParserExpr::Rep(_) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => is_non_failing(&lhs.expr) && is_non_failing(&rhs.expr),
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr) || is_non_failing(&rhs.expr)
        }
        _ => false
    }
}

fn validate_repetition<'i>(rules: &Vec<ParserRule<'i>>) -> Vec<Error<'i, GrammarRule>> {
    rules
        .into_iter()
        .filter_map(|rule| match rule.node.expr {
            ParserExpr::Rep(ref other)
            | ParserExpr::RepOnce(ref other)
            | ParserExpr::RepMin(ref other, _) => {
                if is_non_failing(&other.expr) {
                    Some(Error::CustomErrorSpan {
                        message: "expression inside repetition is non-failing and will repeat \
                                  infinitely"
                            .to_owned(),
                        span: rule.node.span.clone()
                    })
                } else if is_non_progressing(&other.expr) {
                    Some(Error::CustomErrorSpan {
                        message: "expression inside repetition is non-progressing and will repeat \
                                  infinitely"
                            .to_owned(),
                        span: rule.node.span.clone()
                    })
                } else {
                    None
                }
            }
            _ => None
        })
        .collect()
}

fn validate_whitespace_comment<'i>(rules: &Vec<ParserRule<'i>>) -> Vec<Error<'i, GrammarRule>> {
    rules
        .into_iter()
        .filter_map(|rule| {
            if &rule.name == "whitespace" || &rule.name == "comment" {
                if is_non_failing(&rule.node.expr) {
                    Some(Error::CustomErrorSpan {
                        message: format!(
                            "{} is non-failing and will repeat infinitely",
                            &rule.name
                        ),
                        span: rule.node.span.clone()
                    })
                } else if is_non_progressing(&rule.node.expr) {
                    Some(Error::CustomErrorSpan {
                        message: format!(
                            "{} is non-progressing and will repeat infinitely",
                            &rule.name
                        ),
                        span: rule.node.span.clone()
                    })
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect()
}

fn validate_left_recursion<'i>(rules: &Vec<ParserRule<'i>>) -> Vec<Error<'i, GrammarRule>> {
    left_recursion(to_hash_map(rules))
}

fn to_hash_map<'a, 'i>(rules: &'a Vec<ParserRule<'i>>) -> HashMap<Ident, &'a ParserNode<'i>> {
    let mut hash_map = HashMap::new();

    for rule in rules {
        hash_map.insert(rule.name.clone(), &rule.node);
    }

    hash_map
}

fn left_recursion<'i>(rules: HashMap<Ident, &ParserNode<'i>>) -> Vec<Error<'i, GrammarRule>> {
    fn check_expr<'i>(
        node: &ParserNode<'i>,
        rules: &HashMap<Ident, &ParserNode<'i>>,
        trace: &mut Vec<Ident>
    ) -> Option<Error<'i, GrammarRule>> {
        match node.expr {
            ParserExpr::Ident(ref other) => {
                if trace[0] == other {
                    trace.push(other.clone());
                    let chain = trace
                        .iter()
                        .map(|ident| ident.as_ref())
                        .collect::<Vec<_>>()
                        .join(" -> ");

                    return Some(Error::CustomErrorSpan {
                        message: format!(
                            "rule {} is left-recursive ({}); pest::prec_climber might \
                             be useful in this case",
                            node.span.as_str(),
                            chain
                        ),
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
            }
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
    use super::super::parser::{consume_rules, GrammarParser};

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
        validate_pairs(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:1
  |
1 | any = { \"a\" }
  | ^-^
  |
  = any is a pest keyword")]
    fn pest_keyword() {
        let input = "any = { \"a\" }";
        validate_pairs(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        validate_pairs(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        validate_pairs(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }

    #[test]
    fn valid_recursion() {
        let input = "a = { \"\" ~ \"a\"? ~ \"a\"* ~ (\"a\" | \"b\") ~ a }";
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:16
  |
1 | whitespace = { \"\" }
  |                ^^
  |
  = whitespace is non-failing and will repeat infinitely")]
    fn non_failing_whitespace() {
        let input = "whitespace = { \"\" }";
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:13
  |
1 | comment = { soi }
  |             ^-^
  |
  = comment is non-progressing and will repeat infinitely")]
    fn non_progressing_comment() {
        let input = "comment = { soi }";
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { (\"\")* }
  |       ^---^
  |
  = expression inside repetition is non-failing and will repeat infinitely")]
    fn non_failing_repetition() {
        let input = "a = { (\"\")* }";
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
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
        consume_rules(GrammarParser::parse(GrammarRule::grammar_rules, input).unwrap());
    }
}
