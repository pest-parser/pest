// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::iter::Peekable;

use pest::{Error, Parser};
use pest::Span;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use ast::{Expr, Rule as AstRule, RuleType};
use validator;

#[derive(Parser)]
#[grammar = "grammar/pest.pest"]
pub struct PestParser;


pub fn parse<'i>(rule: Rule, data: &'i str) -> Result<Pairs<Rule>, Error<Rule>> {
    PestParser::parse(rule, data)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserRule<'i> {
    pub name: &'i str,
    pub span: Span<'i>,
    pub ty: RuleType,
    pub node: ParserNode<'i>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserNode<'i> {
    pub expr: ParserExpr<'i>,
    pub span: Span<'i>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParserExpr<'i> {
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(&'i str),
    PosPred(Box<ParserNode<'i>>),
    NegPred(Box<ParserNode<'i>>),
    Seq(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Choice(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Opt(Box<ParserNode<'i>>),
    Rep(Box<ParserNode<'i>>),
    RepOnce(Box<ParserNode<'i>>),
    RepExact(Box<ParserNode<'i>>, u32),
    RepMin(Box<ParserNode<'i>>, u32),
    RepMax(Box<ParserNode<'i>>, u32),
    RepMinMax(Box<ParserNode<'i>>, u32, u32),
    Push(Box<ParserNode<'i>>)
}

fn convert_rule<'i>(rule: ParserRule<'i>) -> AstRule {
    match rule {
        ParserRule { name, ty, node, .. } => {
            let expr = convert_node(node);

            AstRule { name, ty, expr }
        }
    }
}

fn convert_node<'i>(node: ParserNode<'i>) -> Expr<'i> {
    match node.expr {
        ParserExpr::Str(string) => Expr::Str(string),
        ParserExpr::Insens(string) => Expr::Insens(string),
        ParserExpr::Range(start, end) => Expr::Range(start, end),
        ParserExpr::Ident(ident) => Expr::Ident(ident),
        ParserExpr::PosPred(node) => Expr::PosPred(Box::new(convert_node(*node))),
        ParserExpr::NegPred(node) => Expr::NegPred(Box::new(convert_node(*node))),
        ParserExpr::Seq(node1, node2) => Expr::Seq(
            Box::new(convert_node(*node1)),
            Box::new(convert_node(*node2))
        ),
        ParserExpr::Choice(node1, node2) => Expr::Choice(
            Box::new(convert_node(*node1)),
            Box::new(convert_node(*node2))
        ),
        ParserExpr::Opt(node) => Expr::Opt(Box::new(convert_node(*node))),
        ParserExpr::Rep(node) => Expr::Rep(Box::new(convert_node(*node))),
        ParserExpr::RepOnce(node) => Expr::RepOnce(Box::new(convert_node(*node))),
        ParserExpr::RepExact(node, num) => Expr::RepExact(Box::new(convert_node(*node)), num),
        ParserExpr::RepMin(node, max) => Expr::RepMin(Box::new(convert_node(*node)), max),
        ParserExpr::RepMax(node, max) => Expr::RepMax(Box::new(convert_node(*node)), max),
        ParserExpr::RepMinMax(node, min, max) => {
            Expr::RepMinMax(Box::new(convert_node(*node)), min, max)
        }
        ParserExpr::Push(node) => Expr::Push(Box::new(convert_node(*node)))
    }
}

pub fn consume_rules<'i>(pairs: Pairs<'i, Rule>) -> Result<Vec<AstRule<'i>>, Vec<Error<'i, Rule>>> {
    let rules = consume_rules_with_spans(pairs);
    let errors = validator::validate_ast(&rules);
    if errors.len() == 0 {
        Ok(rules.into_iter().map(|rule| convert_rule(rule)).collect())
    } else {
        Err(errors)
    }
}

fn consume_rules_with_spans<'i>(pairs: Pairs<'i, Rule>) -> Vec<ParserRule<'i>> {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::choice_operator, Assoc::Left),
        Operator::new(Rule::sequence_operator, Assoc::Left),
    ]);

    pairs
        .filter(|pair| pair.as_rule() == Rule::grammar_rule)
        .map(|pair| {
            let mut pairs = pair.into_inner().peekable();

            let span = pairs.next().unwrap().into_span();
            let name = span.as_str();

            pairs.next().unwrap(); // assignment_operator

            let ty = if pairs.peek().unwrap().as_rule() != Rule::opening_brace {
                match pairs.next().unwrap().as_rule() {
                    Rule::silent_modifier => RuleType::Silent,
                    Rule::atomic_modifier => RuleType::Atomic,
                    Rule::compound_atomic_modifier => RuleType::CompoundAtomic,
                    Rule::non_atomic_modifier => RuleType::NonAtomic,
                    _ => unreachable!()
                }
            } else {
                RuleType::Normal
            };

            pairs.next().unwrap(); // opening_brace

            let node = consume_expr(pairs.next().unwrap().into_inner().peekable(), &climber);

            ParserRule {
                name,
                span,
                ty,
                node
            }
        })
        .collect()
}

fn consume_expr<'i>(
    pairs: Peekable<Pairs<'i, Rule>>,
    climber: &PrecClimber<Rule>
) -> ParserNode<'i> {
    fn unaries<'i>(
        mut pairs: Peekable<Pairs<'i, Rule>>,
        climber: &PrecClimber<Rule>
    ) -> ParserNode<'i> {
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::opening_paren => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: node.expr,
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            Rule::positive_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::PosPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            Rule::negative_predicate_operator => {
                let node = unaries(pairs, climber);
                let end = node.span.end_pos();

                ParserNode {
                    expr: ParserExpr::NegPred(Box::new(node)),
                    span: pair.into_span().start_pos().span(&end)
                }
            }
            other_rule => {
                let node = match other_rule {
                    Rule::expression => consume_expr(pair.into_inner().peekable(), climber),
                    Rule::_push => {
                        let start = pair.clone().into_span().start_pos();
                        let mut pairs = pair.into_inner();
                        pairs.next().unwrap(); // opening_paren
                        let pair = pairs.next().unwrap();

                        let node = consume_expr(pair.into_inner().peekable(), climber);
                        let end = node.span.end_pos();

                        ParserNode {
                            expr: ParserExpr::Push(Box::new(node)),
                            span: start.span(&end)
                        }
                    }
                    Rule::identifier => ParserNode {
                        expr: ParserExpr::Ident(pair.as_str()),
                        span: pair.clone().into_span()
                    },
                    Rule::string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Str(string[1..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    Rule::insensitive_string => {
                        let string = pair.as_str();
                        ParserNode {
                            expr: ParserExpr::Insens(string[2..string.len() - 1].to_owned()),
                            span: pair.clone().into_span()
                        }
                    }
                    Rule::range => {
                        let mut pairs = pair.into_inner();
                        let pair = pairs.next().unwrap();
                        let start = pair.as_str();
                        let start_pos = pair.clone().into_span().start_pos();
                        pairs.next();
                        let pair = pairs.next().unwrap();
                        let end = pair.as_str();
                        let end_pos = pair.clone().into_span().end_pos();

                        ParserNode {
                            expr: ParserExpr::Range(start.to_owned(), end.to_owned()),
                            span: start_pos.span(&end_pos)
                        }
                    }
                    _ => unreachable!()
                };

                pairs.fold(node, |node, pair| {
                    match pair.as_rule() {
                        Rule::optional_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Opt(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::Rep(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_once_operator => {
                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepOnce(Box::new(node)),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_exact => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let number = inner.next().unwrap();
                            let num: u32 = number
                                .as_str()
                                .parse()
                                .expect(&overflow(number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepExact(Box::new(node), num),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_min => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let min_number = inner.next().unwrap();
                            let min: u32 = min_number
                                .as_str()
                                .parse()
                                .expect(&overflow(min_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMin(Box::new(node), min),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_max => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace
                            inner.next().unwrap(); // comma

                            let max_number = inner.next().unwrap();
                            let max: u32 = max_number
                                .as_str()
                                .parse()
                                .expect(&overflow(max_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMax(Box::new(node), max),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::repeat_min_max => {
                            let overflow = |span| {
                                let error: Error<()> = Error::CustomErrorSpan {
                                    message: "number cannot overflow u32".to_owned(),
                                    span
                                };

                                format!("parsing error\n\n{}", error)
                            };

                            let mut inner = pair.clone().into_inner();

                            inner.next().unwrap(); // opening_brace

                            let min_number = inner.next().unwrap();
                            let min: u32 = min_number
                                .as_str()
                                .parse()
                                .expect(&overflow(min_number.into_span()));

                            inner.next().unwrap(); // comma

                            let max_number = inner.next().unwrap();
                            let max: u32 = max_number
                                .as_str()
                                .parse()
                                .expect(&overflow(max_number.into_span()));

                            let start = node.span.start_pos();
                            ParserNode {
                                expr: ParserExpr::RepMinMax(Box::new(node), min, max),
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        Rule::closing_paren => {
                            let start = node.span.start_pos();

                            ParserNode {
                                expr: node.expr,
                                span: start.span(&pair.into_span().end_pos())
                            }
                        }
                        _ => unreachable!()
                    }
                })
            }
        }
    }

    let term = |pair: Pair<'i, Rule>| unaries(pair.into_inner().peekable(), climber);
    let infix =
        |lhs: ParserNode<'i>, op: Pair<'i, Rule>, rhs: ParserNode<'i>| match op.as_rule() {
            Rule::sequence_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Seq(Box::new(lhs), Box::new(rhs)),
                    span: start.span(&end)
                }
            }
            Rule::choice_operator => {
                let start = lhs.span.start_pos();
                let end = rhs.span.end_pos();

                ParserNode {
                    expr: ParserExpr::Choice(Box::new(lhs), Box::new(rhs)),
                    span: start.span(&end)
                }
            }
            _ => unreachable!()
        };

    climber.climb(pairs, term, infix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rules() {
        parses_to! {
            parser: PestParser,
            input: "a = { b } c = { d }",
            rule: Rule::grammar_rules,
            tokens: [
                grammar_rule(0, 9, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    opening_brace(4, 5),
                    expression(6, 7, [
                        term(6, 7, [
                            identifier(6, 7)
                        ])
                    ]),
                    closing_brace(8, 9)
                ]),
                grammar_rule(10, 19, [
                    identifier(10, 11),
                    assignment_operator(12, 13),
                    opening_brace(14, 15),
                    expression(16, 17, [
                        term(16, 17, [
                            identifier(16, 17)
                        ])
                    ]),
                    closing_brace(18, 19)
                ])
            ]
        };
    }

    #[test]
    fn rule() {
        parses_to! {
            parser: PestParser,
            input: "a = ! { b ~ c }",
            rule: Rule::grammar_rule,
            tokens: [
                grammar_rule(0, 15, [
                    identifier(0, 1),
                    assignment_operator(2, 3),
                    non_atomic_modifier(4, 5),
                    opening_brace(6, 7),
                    expression(8, 13, [
                        term(8, 9, [
                            identifier(8, 9)
                        ]),
                        sequence_operator(10, 11),
                        term(12, 13, [
                            identifier(12, 13)
                        ])
                    ]),
                    closing_brace(14, 15)
                ])
            ]
        };
    }

    #[test]
    fn expression() {
        parses_to! {
            parser: PestParser,
            input: "_a | 'a'..'b' ~ !^\"abc\" ~ (d | e)*?",
            rule: Rule::expression,
            tokens: [
                expression(0, 35, [
                    term(0, 2, [
                        identifier(0, 2)
                    ]),
                    choice_operator(3, 4),
                    term(5, 13, [
                        range(5, 13, [
                            character(5, 8, [
                                single_quote(5, 6),
                                single_quote(7, 8)
                            ]),
                            range_operator(8, 10),
                            character(10, 13, [
                                single_quote(10, 11),
                                single_quote(12, 13)
                            ])
                        ])
                    ]),
                    sequence_operator(14, 15),
                    term(16, 23, [
                        negative_predicate_operator(16, 17),
                        insensitive_string(17, 23, [
                            string(18, 23, [
                                quote(18, 19),
                                quote(22, 23)
                            ])
                        ])
                    ]),
                    sequence_operator(24, 25),
                    term(26, 35, [
                        opening_paren(26, 27),
                        expression(27, 32, [
                            term(27, 28, [
                                identifier(27, 28)
                            ]),
                            choice_operator(29, 30),
                            term(31, 32, [
                                identifier(31, 32)
                            ])
                        ]),
                        closing_paren(32, 33),
                        repeat_operator(33, 34),
                        optional_operator(34, 35)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn repeat_exact() {
        parses_to! {
            parser: PestParser,
            input: "{1}",
            rule: Rule::repeat_exact,
            tokens: [
                repeat_exact(0, 3, [
                    opening_brace(0, 1),
                    number(1, 2),
                    closing_brace(2, 3)
                ])
            ]
        };
    }

    #[test]
    fn repeat_min() {
        parses_to! {
            parser: PestParser,
            input: "{2,}",
            rule: Rule::repeat_min,
            tokens: [
                repeat_min(0, 4, [
                    opening_brace(0,1),
                    number(1,2),
                    comma(2,3),
                    closing_brace(3,4)
                ])
            ]
        }
    }

    #[test]
    fn repeat_max() {
        parses_to! {
            parser: PestParser,
            input: "{, 3}",
            rule: Rule::repeat_max,
            tokens: [
                repeat_max(0, 5, [
                    opening_brace(0,1),
                    comma(1,2),
                    number(3,4),
                    closing_brace(4,5)
                ])
            ]
        }
    }

    #[test]
    fn repeat_min_max() {
        parses_to! {
            parser: PestParser,
            input: "{1, 2}",
            rule: Rule::repeat_min_max,
            tokens: [
                repeat_min_max(0, 6, [
                    opening_brace(0, 1),
                    number(1, 2),
                    comma(2, 3),
                    number(4, 5),
                    closing_brace(5, 6)
                ])
            ]
        };
    }

    #[test]
    fn push() {
        parses_to! {
            parser: PestParser,
            input: "push ( a )",
            rule: Rule::push,
            tokens: [
                push(0, 10, [
                    opening_paren(5, 6),
                    expression(7, 8, [
                        term(7, 8, [
                            identifier(7, 8)
                        ])
                    ]),
                    closing_paren(9, 10)
                ])
            ]
        };
    }

    #[test]
    fn identifier() {
        parses_to! {
            parser: PestParser,
            input: "_a8943",
            rule: Rule::identifier,
            tokens: [
                identifier(0, 6)
            ]
        };
    }

    #[test]
    fn string() {
        parses_to! {
            parser: PestParser,
            input: "\"aaaaa\\n\\r\\t\\\\\\0\\'\\\"\\x0F\\u{123abC}\\u{12}aaaaa\"",
            rule: Rule::string,
            tokens: [
                string(0, 46, [
                    quote(0, 1),
                    quote(45, 46)
                ])
            ]
        };
    }

    #[test]
    fn insensitive_string() {
        parses_to! {
            parser: PestParser,
            input: "^  \"\\\"hi\"",
            rule: Rule::insensitive_string,
            tokens: [
                insensitive_string(0, 9, [
                    string(3, 9, [
                        quote(3, 4),
                        quote(8, 9)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn range() {
        parses_to! {
            parser: PestParser,
            input: "'\\n' .. '\\x1a'",
            rule: Rule::range,
            tokens: [
                range(0, 14, [
                    character(0, 4, [
                        single_quote(0, 1),
                        single_quote(3, 4)
                    ]),
                    range_operator(5, 7),
                    character(8, 14, [
                        single_quote(8, 9),
                        single_quote(13, 14)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn character() {
        parses_to! {
            parser: PestParser,
            input: "'\\u{123abC}'",
            rule: Rule::character,
            tokens: [
                character(0, 12, [
                    single_quote(0, 1),
                    single_quote(11, 12)
                ])
            ]
        };
    }

    #[test]
    fn number() {
        parses_to! {
            parser: PestParser,
            input: "0123",
            rule: Rule::number,
            tokens: [
                number(0, 4)
            ]
        };
    }

    #[test]
    fn comment() {
        parses_to! {
            parser: PestParser,
            input: "a ~    // asda\n b",
            rule: Rule::expression,
            tokens: [
                expression(0, 17, [
                    term(0, 1, [
                        identifier(0, 1)
                    ]),
                    sequence_operator(2, 3),
                    term(16, 17, [
                        identifier(16, 17)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn wrong_identifier() {
        fails_with! {
            parser: PestParser,
            input: "0",
            rule: Rule::grammar_rules,
            positives: vec![Rule::identifier],
            negatives: vec![],
            pos: 0
        };
    }

    #[test]
    fn missing_assignment_operator() {
        fails_with! {
            parser: PestParser,
            input: "a {}",
            rule: Rule::grammar_rules,
            positives: vec![Rule::assignment_operator],
            negatives: vec![],
            pos: 2
        };
    }

    #[test]
    fn wrong_modifier() {
        fails_with! {
            parser: PestParser,
            input: "a = *{}",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::silent_modifier,
                Rule::atomic_modifier,
                Rule::compound_atomic_modifier,
                Rule::non_atomic_modifier,
                Rule::opening_brace
            ],
            negatives: vec![],
            pos: 4
        };
    }

    #[test]
    fn missing_opening_brace() {
        fails_with! {
            parser: PestParser,
            input: "a = _",
            rule: Rule::grammar_rules,
            positives: vec![Rule::opening_brace],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn empty_rule() {
        fails_with! {
            parser: PestParser,
            input: "a = {}",
            rule: Rule::grammar_rules,
            positives: vec![Rule::expression],
            negatives: vec![],
            pos: 5
        };
    }

    #[test]
    fn missing_rhs() {
        fails_with! {
            parser: PestParser,
            input: "a = { b ~ }",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_paren,
                Rule::positive_predicate_operator,
                Rule::negative_predicate_operator,
                Rule::push,
                Rule::identifier,
                Rule::quote,
                Rule::insensitive_string,
                Rule::single_quote
            ],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn wrong_op() {
        fails_with! {
            parser: PestParser,
            input: "a = { b % }",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_brace,
                Rule::closing_brace,
                Rule::sequence_operator,
                Rule::choice_operator,
                Rule::optional_operator,
                Rule::repeat_operator,
                Rule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn missing_closing_paren() {
        fails_with! {
            parser: PestParser,
            input: "a = { (b }",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_brace,
                Rule::closing_paren,
                Rule::sequence_operator,
                Rule::choice_operator,
                Rule::optional_operator,
                Rule::repeat_operator,
                Rule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn missing_term() {
        fails_with! {
            parser: PestParser,
            input: "a = { ! }",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_paren,
                Rule::positive_predicate_operator,
                Rule::negative_predicate_operator,
                Rule::push,
                Rule::identifier,
                Rule::quote,
                Rule::insensitive_string,
                Rule::single_quote
            ],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn string_missing_ending_quote() {
        fails_with! {
            parser: PestParser,
            input: "a = { \" }",
            rule: Rule::grammar_rules,
            positives: vec![Rule::quote],
            negatives: vec![],
            pos: 9
        };
    }

    #[test]
    fn insensitive_missing_string() {
        fails_with! {
            parser: PestParser,
            input: "a = { ^ }",
            rule: Rule::grammar_rules,
            positives: vec![Rule::string],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn char_missing_ending_single_quote() {
        fails_with! {
            parser: PestParser,
            input: "a = { \' }",
            rule: Rule::grammar_rules,
            positives: vec![Rule::single_quote],
            negatives: vec![],
            pos: 8
        };
    }

    #[test]
    fn range_missing_range_operator() {
        fails_with! {
            parser: PestParser,
            input: "a = { \'a\' }",
            rule: Rule::grammar_rules,
            positives: vec![Rule::range_operator],
            negatives: vec![],
            pos: 10
        };
    }

    #[test]
    fn wrong_postfix() {
        fails_with! {
            parser: PestParser,
            input: "a = { a& }",
            rule: Rule::grammar_rules,
            positives: vec![
                Rule::opening_brace,
                Rule::closing_brace,
                Rule::sequence_operator,
                Rule::choice_operator,
                Rule::optional_operator,
                Rule::repeat_operator,
                Rule::repeat_once_operator
            ],
            negatives: vec![],
            pos: 7
        };
    }

    #[test]
    fn ast() {
        let input =
            "rule = _{ a{1} ~ \"a\"{3,} ~ b{, 2} ~ \"b\"{1, 2} | !(^\"c\" | push('d'..'e'))?* }";

        let pairs = PestParser::parse(Rule::grammar_rules, input).unwrap();
        let ast = consume_rules_with_spans(pairs);
        let ast: Vec<_> = ast.into_iter().map(|rule| convert_rule(rule)).collect();

        assert_eq!(
            ast,
            vec![
                AstRule {
                    name: "rule",
                    ty: RuleType::Silent,
                    expr: Expr::Choice(
                        Box::new(Expr::Seq(
                            Box::new(Expr::Seq(
                                Box::new(Expr::Seq(
                                    Box::new(Expr::RepExact(
                                        Box::new(Expr::Ident("a")),
                                        1
                                    )),
                                    Box::new(Expr::RepMin(Box::new(Expr::Str("a".to_owned())), 3))
                                )),
                                Box::new(Expr::RepMax(Box::new(Expr::Ident("b")), 2))
                            )),
                            Box::new(Expr::RepMinMax(Box::new(Expr::Str("b".to_owned())), 1, 2))
                        )),
                        Box::new(Expr::NegPred(Box::new(Expr::Rep(Box::new(Expr::Opt(
                            Box::new(Expr::Choice(
                                Box::new(Expr::Insens("c".to_owned())),
                                Box::new(Expr::Push(Box::new(Expr::Range(
                                    "'d'".to_owned(),
                                    "'e'".to_owned()
                                ))))
                            ))
                        ))))))
                    )
                },
            ]
        );
    }
}
