// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Helpers for validating pest grammars that could help with debugging
//! and provide a more user-friendly error message.

use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use pest::error::{Error, ErrorVariant, InputLocation};
use pest::iterators::Pairs;
use pest::unicode::unicode_property_names;
use pest::Span;

use crate::parser::{ParserExpr, ParserNode, ParserRule, Rule};

static RUST_KEYWORDS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
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

static PEST_KEYWORDS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    [
        "_", "ANY", "DROP", "EOI", "PEEK", "PEEK_ALL", "POP", "POP_ALL", "PUSH", "SOI",
    ]
    .iter()
    .cloned()
    .collect()
});

static BUILTINS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
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
    .chain(unicode_property_names())
    .collect::<HashSet<&str>>()
});

/// It checks the parsed grammar for common mistakes:
/// - using Pest keywords
/// - duplicate rules
/// - undefined rules
///
/// It returns a `Result` with a `Vec` of `Error`s if any of the above is found.
/// If no errors are found, it returns the vector of names of used builtin rules.
pub fn validate_pairs(pairs: Pairs<'_, Rule>) -> Result<Vec<&str>, Vec<Error<Rule>>> {
    let definitions: Vec<_> = pairs
        .clone()
        .filter(|pair| pair.as_rule() == Rule::grammar_rule)
        .map(|pair| pair.into_inner().next().unwrap())
        .filter(|pair| pair.as_rule() != Rule::line_doc)
        .map(|pair| pair.as_span())
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

/// Validates that the given `definitions` do not contain any Rust keywords.
#[allow(clippy::ptr_arg)]
#[deprecated = "Rust keywords are no longer restricted from the pest grammar"]
pub fn validate_rust_keywords(definitions: &Vec<Span<'_>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if RUST_KEYWORDS.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("{name} is a rust keyword"),
                },
                *definition,
            ))
        }
    }

    errors
}

/// Validates that the given `definitions` do not contain any Pest keywords.
#[allow(clippy::ptr_arg)]
pub fn validate_pest_keywords(definitions: &Vec<Span<'_>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    for definition in definitions {
        let name = definition.as_str();

        if PEST_KEYWORDS.contains(name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("{name} is a pest keyword"),
                },
                *definition,
            ))
        }
    }

    errors
}

/// Validates that the given `definitions` do not contain any duplicate rules.
#[allow(clippy::ptr_arg)]
pub fn validate_already_defined(definitions: &Vec<Span<'_>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];
    let mut defined = HashSet::new();

    for definition in definitions {
        let name = definition.as_str();

        if defined.contains(&name) {
            errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("rule {name} already defined"),
                },
                *definition,
            ))
        } else {
            defined.insert(name);
        }
    }

    errors
}

/// Validates that the given `definitions` do not contain any undefined rules.
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
                    message: format!("rule {name} is undefined"),
                },
                *rule,
            ))
        }
    }

    errors
}

/// Validates the abstract syntax tree for common mistakes:
/// - infinite repetitions, including ones caused by the stack (e.g. `POP_ALL ~ PEEK_ALL*`)
/// - choices that cannot be reached
/// - left recursion
#[allow(clippy::ptr_arg)]
pub fn validate_ast<'a, 'i: 'a>(rules: &'a Vec<ParserRule<'i>>) -> Vec<Error<Rule>> {
    let mut errors = vec![];

    // WARNING: validate_{repetition,choice,whitespace_comment}
    // use is_non_failing and is_non_progressing breaking assumptions:
    // - for every `ParserExpr::RepMinMax(inner,min,max)`,
    //   `min<=max` was not checked
    // - left recursion was not checked
    // - Every expression might not be checked
    errors.extend(validate_repetition(rules));
    errors.extend(validate_stack_repetition(rules));
    errors.extend(validate_choices(rules));
    errors.extend(validate_whitespace_comment(rules));
    errors.extend(validate_left_recursion(rules));
    #[cfg(feature = "grammar-extras")]
    errors.extend(validate_tag_silent_rules(rules));

    errors.sort_by_key(|error| match error.location {
        InputLocation::Span(span) => span,
        _ => unreachable!(),
    });

    errors
}

#[cfg(feature = "grammar-extras")]
fn validate_tag_silent_rules<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    use crate::ast::RuleType;

    fn to_type_hash_map<'a, 'i: 'a>(
        rules: &'a [ParserRule<'i>],
    ) -> HashMap<String, (&'a ParserNode<'i>, RuleType)> {
        rules
            .iter()
            .map(|r| (r.name.clone(), (&r.node, r.ty)))
            .collect()
    }
    let mut result = vec![];

    fn check_silent_builtin<'a, 'i: 'a>(
        expr: &ParserExpr<'i>,
        rules_ref: &HashMap<String, (&'a ParserNode<'i>, RuleType)>,
        span: Span<'a>,
    ) -> Option<Error<Rule>> {
        match &expr {
            ParserExpr::Ident(rule_name) => {
                let rule = rules_ref.get(rule_name);
                if matches!(rule, Some((_, RuleType::Silent))) {
                    return Some(Error::<Rule>::new_from_span(
                        ErrorVariant::CustomError {
                            message: "tags on silent rules will not appear in the output"
                                .to_owned(),
                        },
                        span,
                    ));
                } else if BUILTINS.contains(rule_name.as_str()) {
                    return Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "tags on built-in rules will not appear in the output"
                                .to_owned(),
                        },
                        span,
                    ));
                }
            }
            ParserExpr::Rep(node)
            | ParserExpr::RepMinMax(node, _, _)
            | ParserExpr::RepMax(node, _)
            | ParserExpr::RepMin(node, _)
            | ParserExpr::RepOnce(node)
            | ParserExpr::RepExact(node, _)
            | ParserExpr::Opt(node)
            | ParserExpr::Push(node)
            | ParserExpr::PosPred(node)
            | ParserExpr::NegPred(node) => {
                return check_silent_builtin(&node.expr, rules_ref, span);
            }
            _ => {}
        };
        None
    }

    let rules_map = to_type_hash_map(rules);
    for rule in rules {
        let rules_ref = &rules_map;
        let mut errors = rule.node.clone().filter_map_top_down(|node1| {
            if let ParserExpr::NodeTag(node2, _) = node1.expr {
                check_silent_builtin(&node2.expr, rules_ref, node1.span)
            } else {
                None
            }
        });
        result.append(&mut errors);
    }
    result
}

/// Checks if `expr` is non-progressing, that is the expression does not
/// consume any input or any stack. This includes expressions matching the empty input,
/// `SOI` and ̀ `EOI`, predicates and repetitions.
///
/// # Example
///
/// ```pest
/// not_progressing_1 = { "" }
/// not_progressing_2 = { "a"? }
/// not_progressing_3 = { !"a" }
/// ```
///
/// # Assumptions
/// - In `ParserExpr::RepMinMax(inner,min,max)`, `min<=max`
/// - All rules identifiers have a matching definition
/// - There is no left-recursion (if only this one is broken returns false)
/// - Every expression is being checked
fn is_non_progressing<'i>(
    expr: &ParserExpr<'i>,
    rules: &HashMap<String, &ParserNode<'i>>,
    trace: &mut Vec<String>,
) -> bool {
    match *expr {
        ParserExpr::Str(ref string) | ParserExpr::Insens(ref string) => string.is_empty(),
        ParserExpr::Ident(ref ident) => {
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
                // else
                // the ident is
                // - "POP","PEEK" => false
                //      the slice being checked is not non_progressing since every
                //      PUSH is being checked (assumption 4) and the expr
                //      of a PUSH has to be non_progressing.
                // - "POPALL", "PEEKALL" => false
                //      same as "POP", "PEEK" unless the following:
                //      BUG: if the stack is empty they are non_progressing
                // - "DROP" => false doesn't consume the input but consumes the stack,
                // - "ANY", "ASCII_*", UNICODE categories, "NEWLINE" => false
                // - referring to another rule that is undefined (breaks assumption)
            }
            // else referring to another rule that was already seen.
            //    this happens only if there is a left-recursion
            //    that is only if an assumption is broken,
            //    WARNING: we can choose to return false, but that might
            //    cause bugs into the left_recursion check

            false
        }
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr, rules, trace)
                && is_non_progressing(&rhs.expr, rules, trace)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_progressing(&lhs.expr, rules, trace)
                || is_non_progressing(&rhs.expr, rules, trace)
        }
        // WARNING: the predicate indeed won't make progress on input but  it
        // might progress on the stack
        // ex: @{ PUSH(ANY) ~ (&(DROP))* ~ ANY }, input="AA"
        //     Notice that this is ex not working as of now, the debugger seems
        //     to run into an infinite loop on it
        ParserExpr::PosPred(_) | ParserExpr::NegPred(_) => true,
        ParserExpr::Rep(_) | ParserExpr::Opt(_) | ParserExpr::RepMax(_, _) => true,
        // it either always fail (failing is progressing)
        // or always match at least a character
        ParserExpr::Range(_, _) => false,
        ParserExpr::PeekSlice(_, _) => {
            // the slice being checked is not non_progressing since every
            // PUSH is being checked (assumption 4) and the expr
            // of a PUSH has to be non_progressing.
            // BUG: if the slice is of size 0, or the stack is not large
            // enough it might be non-progressing
            false
        }

        ParserExpr::RepExact(ref inner, min)
        | ParserExpr::RepMin(ref inner, min)
        | ParserExpr::RepMinMax(ref inner, min, _) => {
            min == 0 || is_non_progressing(&inner.expr, rules, trace)
        }
        ParserExpr::Push(ref inner) => is_non_progressing(&inner.expr, rules, trace),
        #[cfg(feature = "grammar-extras")]
        ParserExpr::PushLiteral(_) => true,
        ParserExpr::RepOnce(ref inner) => is_non_progressing(&inner.expr, rules, trace),
        #[cfg(feature = "grammar-extras")]
        ParserExpr::NodeTag(ref inner, _) => is_non_progressing(&inner.expr, rules, trace),
    }
}

/// Checks if `expr` is non-failing, that is it matches any input.
///
/// # Example
///
/// ```pest
/// non_failing_1 = { "" }
/// ```
///
/// # Assumptions
/// - In `ParserExpr::RepMinMax(inner,min,max)`, `min<=max`
/// - In `ParserExpr::PeekSlice(max,Some(min))`, `max>=min`
/// - All rules identifiers have a matching definition
/// - There is no left-recursion
/// - All rules are being checked
fn is_non_failing<'i>(
    expr: &ParserExpr<'i>,
    rules: &HashMap<String, &ParserNode<'i>>,
    trace: &mut Vec<String>,
) -> bool {
    match *expr {
        ParserExpr::Str(ref string) | ParserExpr::Insens(ref string) => string.is_empty(),
        ParserExpr::Ident(ref ident) => {
            if !trace.contains(ident) {
                if let Some(node) = rules.get(ident) {
                    trace.push(ident.clone());
                    let result = is_non_failing(&node.expr, rules, trace);
                    trace.pop().unwrap();

                    result
                } else {
                    // else
                    // the ident is
                    // - "POP","PEEK" => false
                    //      the slice being checked is not non_failing since every
                    //      PUSH is being checked (assumption 4) and the expr
                    //      of a PUSH has to be non_failing.
                    // - "POP_ALL", "PEEK_ALL" => false
                    //      same as "POP", "PEEK" unless the following:
                    //      BUG: if the stack is empty they are non_failing
                    // - "DROP" => false
                    // - "ANY", "ASCII_*", UNICODE categories, "NEWLINE",
                    //      "SOI", "EOI" => false
                    // - referring to another rule that is undefined (breaks assumption)
                    //      WARNING: might want to introduce a panic or report the error
                    false
                }
            } else {
                // referring to another rule R that was already seen
                // WARNING: this might mean there is a circular non-failing path
                //   it's not obvious whether this can happen without left-recursion
                //   and thus breaking the assumption. Until there is answer to
                //   this, to avoid changing behaviour we return:
                false
            }
        }
        ParserExpr::Opt(_) => true,
        ParserExpr::Rep(_) => true,
        ParserExpr::RepMax(_, _) => true,
        ParserExpr::Seq(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr, rules, trace) && is_non_failing(&rhs.expr, rules, trace)
        }
        ParserExpr::Choice(ref lhs, ref rhs) => {
            is_non_failing(&lhs.expr, rules, trace) || is_non_failing(&rhs.expr, rules, trace)
        }
        // it either always fail
        // or always match at least a character
        ParserExpr::Range(_, _) => false,
        ParserExpr::PeekSlice(_, _) => {
            // the slice being checked is not non_failing since every
            // PUSH is being checked (assumption 4) and the expr
            // of a PUSH has to be non_failing.
            // BUG: if the slice is of size 0, or the stack is not large
            // enough it might be non-failing
            false
        }
        ParserExpr::RepExact(ref inner, min)
        | ParserExpr::RepMin(ref inner, min)
        | ParserExpr::RepMinMax(ref inner, min, _) => {
            min == 0 || is_non_failing(&inner.expr, rules, trace)
        }
        // BUG: the predicate may always fail, resulting in this expr non_failing
        // ex of always failing predicates :
        //     @{EOI ~ ANY | ANY ~ SOI | &("A") ~ &("B") | 'z'..'a'}
        ParserExpr::NegPred(_) => false,
        ParserExpr::RepOnce(ref inner) => is_non_failing(&inner.expr, rules, trace),
        ParserExpr::Push(ref inner) | ParserExpr::PosPred(ref inner) => {
            is_non_failing(&inner.expr, rules, trace)
        }
        #[cfg(feature = "grammar-extras")]
        ParserExpr::PushLiteral(_) => true,
        #[cfg(feature = "grammar-extras")]
        ParserExpr::NodeTag(ref inner, _) => is_non_failing(&inner.expr, rules, trace),
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
                                rule.name
                            ),
                        },
                        rule.node.span,
                    ))
                } else if is_non_progressing(&rule.node.expr, &map, &mut vec![]) {
                    Some(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "{} is non-progressing and will repeat infinitely",
                                rule.name
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

/// The part of the stack below the entries an [`AbsStack`] tracks.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum StackBase {
    /// No entries.
    Empty,
    /// Only empty-string entries, possibly none.
    Blank,
    /// Anything.
    Any,
}

/// What is known about the stack at a point of a rule: an unknown `base` with `top`
/// empty-string entries pushed on it.
///
/// Only empty strings are tracked on top because they are the only entries that `PEEK`,
/// `POP`, `PEEK_ALL` and `POP_ALL` can match without consuming input.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct AbsStack {
    base: StackBase,
    top: u8,
    /// `base` still holds what it held when the current repetition iteration started.
    base_intact: bool,
}

impl AbsStack {
    /// Most empty-string entries tracked on top; beyond that the state is given up.
    const MAX_TOP: u8 = 8;

    const UNKNOWN: AbsStack = AbsStack {
        base: StackBase::Any,
        top: 0,
        base_intact: false,
    };

    fn join(self, other: AbsStack) -> AbsStack {
        if self == other {
            return self;
        }
        let base = match (self.base, other.base) {
            (a, b) if a == b => a,
            (StackBase::Any, _) | (_, StackBase::Any) => StackBase::Any,
            _ => StackBase::Blank,
        };
        if self.top == other.top {
            AbsStack {
                base,
                top: self.top,
                base_intact: self.base_intact && other.base_intact,
            }
        } else if base == StackBase::Any {
            AbsStack::UNKNOWN
        } else {
            // Different numbers of empty strings on a blank base: all entries are blank.
            AbsStack {
                base: StackBase::Blank,
                top: 0,
                base_intact: false,
            }
        }
    }

    fn push_blank(self) -> Option<AbsStack> {
        (self.top < AbsStack::MAX_TOP).then_some(AbsStack {
            top: self.top + 1,
            ..self
        })
    }

    /// Every entry is an empty string.
    fn all_blank(self) -> bool {
        self.base != StackBase::Any
    }

    /// The state after `POP_ALL` succeeds.
    fn popped_all(self) -> AbsStack {
        AbsStack {
            base: StackBase::Empty,
            top: 0,
            base_intact: self.base_intact && self.base == StackBase::Empty,
        }
    }
}

/// Finds repetitions that can never end because of the stack: their body always succeeds
/// without consuming input and, after at most a few iterations, without changing the stack.
/// `is_non_progressing` treats the stack builtins as progressing, which misses for example
/// `POP_ALL ~ PEEK_ALL*` (`PEEK_ALL` matches nothing on an empty stack) and
/// `(PUSH("") ~ POP)*`.
///
/// Every rule is analysed as if entered with an unknown stack, so a repetition is only
/// reported when it loops whatever the stack holds when it is reached. A bare `PEEK_ALL*`
/// is not reported: it terminates when the stack holds a non-empty string.
fn validate_stack_repetition<'a, 'i: 'a>(rules: &'a [ParserRule<'i>]) -> Vec<Error<Rule>> {
    let map = to_hash_map(rules);

    // Implicit whitespace runs between the steps of non-atomic rules. If it can change the
    // stack, iterations that look unchanged here might not be, so report nothing.
    let mut seen = HashSet::new();
    if ["WHITESPACE", "COMMENT"]
        .iter()
        .any(|name| rule_modifies_stack(name, &map, &mut seen))
    {
        return vec![];
    }

    let mut analysis = StackAnalysis {
        rules: &map,
        post_memo: HashMap::new(),
        exact_memo: HashMap::new(),
        post_active: HashSet::new(),
        exact_active: HashSet::new(),
    };
    let mut errors = vec![];
    let entry = AbsStack {
        base: StackBase::Any,
        top: 0,
        base_intact: true,
    };
    for rule in rules {
        analysis.walk(&rule.node, entry, &mut errors);
    }
    errors
}

fn rule_modifies_stack(
    name: &str,
    rules: &HashMap<String, &ParserNode<'_>>,
    seen: &mut HashSet<String>,
) -> bool {
    match name {
        "POP" | "DROP" | "POP_ALL" => true,
        _ => match rules.get(name) {
            Some(node) if seen.insert(name.to_owned()) => {
                expr_modifies_stack(&node.expr, rules, seen)
            }
            _ => false,
        },
    }
}

fn expr_modifies_stack(
    expr: &ParserExpr<'_>,
    rules: &HashMap<String, &ParserNode<'_>>,
    seen: &mut HashSet<String>,
) -> bool {
    match expr {
        ParserExpr::Push(_) => true,
        #[cfg(feature = "grammar-extras")]
        ParserExpr::PushLiteral(_) => true,
        ParserExpr::Ident(name) => rule_modifies_stack(name, rules, seen),
        ParserExpr::Str(_)
        | ParserExpr::Insens(_)
        | ParserExpr::Range(_, _)
        | ParserExpr::PeekSlice(_, _) => false,
        // Predicates restore the stack.
        ParserExpr::PosPred(_) | ParserExpr::NegPred(_) => false,
        ParserExpr::Seq(lhs, rhs) | ParserExpr::Choice(lhs, rhs) => {
            expr_modifies_stack(&lhs.expr, rules, seen)
                || expr_modifies_stack(&rhs.expr, rules, seen)
        }
        ParserExpr::Opt(inner)
        | ParserExpr::Rep(inner)
        | ParserExpr::RepOnce(inner)
        | ParserExpr::RepExact(inner, _)
        | ParserExpr::RepMin(inner, _)
        | ParserExpr::RepMax(inner, _)
        | ParserExpr::RepMinMax(inner, _, _) => expr_modifies_stack(&inner.expr, rules, seen),
        #[cfg(feature = "grammar-extras")]
        ParserExpr::NodeTag(inner, _) => expr_modifies_stack(&inner.expr, rules, seen),
    }
}

struct StackAnalysis<'a, 'i> {
    rules: &'a HashMap<String, &'a ParserNode<'i>>,
    post_memo: HashMap<(String, AbsStack), AbsStack>,
    exact_memo: HashMap<(String, AbsStack), Option<AbsStack>>,
    post_active: HashSet<(String, AbsStack)>,
    exact_active: HashSet<(String, AbsStack)>,
}

impl<'i> StackAnalysis<'_, 'i> {
    /// Visits `node` entered with stack `s`, reports looping repetitions in it, and returns
    /// the stack after `node` succeeds.
    fn walk(
        &mut self,
        node: &ParserNode<'i>,
        s: AbsStack,
        errors: &mut Vec<Error<Rule>>,
    ) -> AbsStack {
        match &node.expr {
            ParserExpr::Seq(lhs, rhs) => {
                let mid = self.walk(lhs, s, errors);
                self.walk(rhs, mid, errors)
            }
            ParserExpr::Choice(lhs, rhs) => {
                let l = self.walk(lhs, s, errors);
                let r = self.walk(rhs, s, errors);
                l.join(r)
            }
            ParserExpr::Opt(inner) => s.join(self.walk(inner, s, errors)),
            ParserExpr::Rep(inner) | ParserExpr::RepOnce(inner) | ParserExpr::RepMin(inner, _) => {
                if self.loops_forever(&inner.expr, s)
                    && !is_non_failing(&inner.expr, self.rules, &mut vec![])
                    && !is_non_progressing(&inner.expr, self.rules, &mut vec![])
                {
                    errors.push(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "expression inside repetition is non-progressing and will \
                                      repeat infinitely"
                                .to_owned(),
                        },
                        node.span,
                    ));
                }
                let every = self.post_repeated(&inner.expr, s);
                self.walk(inner, every, errors);
                every
            }
            ParserExpr::RepExact(inner, _)
            | ParserExpr::RepMax(inner, _)
            | ParserExpr::RepMinMax(inner, _, _) => {
                let every = self.post_repeated(&inner.expr, s);
                self.walk(inner, every, errors);
                every
            }
            ParserExpr::PosPred(inner) | ParserExpr::NegPred(inner) => {
                self.walk(inner, s, errors);
                s
            }
            ParserExpr::Push(inner) => {
                self.walk(inner, s, errors);
                self.post(&node.expr, s)
            }
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.walk(inner, s, errors),
            _ => self.post(&node.expr, s),
        }
    }

    /// A repetition of `body` entered with stack `s` never ends: from every stack `s` allows,
    /// `body` succeeds without consuming input, and within a few iterations it leaves the
    /// stack unchanged, so every later iteration repeats the same step.
    fn loops_forever(&mut self, body: &ParserExpr<'i>, s: AbsStack) -> bool {
        let mut cur = AbsStack {
            base_intact: true,
            ..s
        };
        for _ in 0..4 {
            match self.exact(body, cur) {
                None => return false,
                Some(out) if out.base_intact && out.top == cur.top => return true,
                Some(out) => {
                    cur = AbsStack {
                        base_intact: true,
                        ..out
                    }
                }
            }
        }
        false
    }

    /// The stack at the start of any iteration of a repetition of `body` entered with `s`
    /// (also covers zero iterations).
    fn post_repeated(&mut self, body: &ParserExpr<'i>, s: AbsStack) -> AbsStack {
        let mut cur = s;
        loop {
            let next = cur.join(self.post(body, cur));
            if next == cur {
                return cur;
            }
            cur = next;
        }
    }

    /// The stack after `expr` succeeds when entered with `s` (an over-approximation).
    fn post(&mut self, expr: &ParserExpr<'i>, s: AbsStack) -> AbsStack {
        match expr {
            ParserExpr::Str(_)
            | ParserExpr::Insens(_)
            | ParserExpr::Range(_, _)
            | ParserExpr::PeekSlice(_, _)
            | ParserExpr::PosPred(_)
            | ParserExpr::NegPred(_) => s,
            ParserExpr::Ident(name) => match name.as_str() {
                "POP_ALL" => s.popped_all(),
                "PEEK" | "PEEK_ALL" => s,
                "POP" | "DROP" => {
                    if s.top > 0 {
                        AbsStack {
                            top: s.top - 1,
                            ..s
                        }
                    } else {
                        match s.base {
                            // POP panics and DROP fails on an empty stack.
                            StackBase::Empty => s,
                            StackBase::Blank => AbsStack {
                                base: StackBase::Blank,
                                top: 0,
                                base_intact: false,
                            },
                            StackBase::Any => AbsStack::UNKNOWN,
                        }
                    }
                }
                _ => {
                    if self.rules.contains_key(name) {
                        self.post_rule(name, s)
                    } else {
                        // other builtins do not touch the stack
                        s
                    }
                }
            },
            ParserExpr::Seq(lhs, rhs) => {
                let mid = self.post(&lhs.expr, s);
                self.post(&rhs.expr, mid)
            }
            ParserExpr::Choice(lhs, rhs) => {
                let l = self.post(&lhs.expr, s);
                l.join(self.post(&rhs.expr, s))
            }
            ParserExpr::Opt(inner) => s.join(self.post(&inner.expr, s)),
            ParserExpr::Rep(inner)
            | ParserExpr::RepOnce(inner)
            | ParserExpr::RepExact(inner, _)
            | ParserExpr::RepMin(inner, _)
            | ParserExpr::RepMax(inner, _)
            | ParserExpr::RepMinMax(inner, _, _) => self.post_repeated(&inner.expr, s),
            ParserExpr::Push(inner) => match self.exact(&inner.expr, s) {
                // `inner` matched nothing, so an empty string was pushed.
                Some(after) => after.push_blank().unwrap_or(AbsStack::UNKNOWN),
                None => AbsStack::UNKNOWN,
            },
            #[cfg(feature = "grammar-extras")]
            ParserExpr::PushLiteral(string) if string.is_empty() => {
                s.push_blank().unwrap_or(AbsStack::UNKNOWN)
            }
            #[cfg(feature = "grammar-extras")]
            ParserExpr::PushLiteral(_) => AbsStack::UNKNOWN,
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.post(&inner.expr, s),
        }
    }

    fn post_rule(&mut self, name: &str, s: AbsStack) -> AbsStack {
        let key = (name.to_owned(), s);
        if let Some(&memo) = self.post_memo.get(&key) {
            return memo;
        }
        if !self.post_active.insert(key.clone()) {
            // recursion: give up on what the stack holds
            return AbsStack::UNKNOWN;
        }
        let node = self.rules[name];
        let result = self.post(&node.expr, s);
        let _ = self.post_active.remove(&key);
        let _ = self.post_memo.insert(key, result);
        result
    }

    /// `Some(stack after)` if, from every stack `s` allows and on any input, `expr` succeeds
    /// without consuming input; `None` if that is not certain.
    fn exact(&mut self, expr: &ParserExpr<'i>, s: AbsStack) -> Option<AbsStack> {
        match expr {
            ParserExpr::Str(string) | ParserExpr::Insens(string) => string.is_empty().then_some(s),
            ParserExpr::Range(_, _) | ParserExpr::NegPred(_) => None,
            ParserExpr::Ident(name) => match name.as_str() {
                "PEEK_ALL" => s.all_blank().then_some(s),
                "POP_ALL" => s.all_blank().then(|| s.popped_all()),
                "PEEK" => (s.top > 0).then_some(s),
                "POP" | "DROP" => (s.top > 0).then(|| AbsStack {
                    top: s.top - 1,
                    ..s
                }),
                _ => {
                    if self.rules.contains_key(name) {
                        self.exact_rule(name, s)
                    } else {
                        // other builtins (ANY, SOI, EOI, ...) can fail or consume
                        None
                    }
                }
            },
            // `PEEK[..]` is `PEEK_ALL` matched bottom to top; other slices can be out of range.
            ParserExpr::PeekSlice(0, None) => s.all_blank().then_some(s),
            ParserExpr::PeekSlice(_, _) => None,
            ParserExpr::PosPred(inner) => self.exact(&inner.expr, s).map(|_| s),
            ParserExpr::Seq(lhs, rhs) => {
                let mid = self.exact(&lhs.expr, s)?;
                self.exact(&rhs.expr, mid)
            }
            // `lhs` always succeeds, so `rhs` is never tried.
            ParserExpr::Choice(lhs, _) => self.exact(&lhs.expr, s),
            ParserExpr::Opt(inner) => self.exact(&inner.expr, s),
            ParserExpr::Rep(_)
            | ParserExpr::RepOnce(_)
            | ParserExpr::RepExact(_, _)
            | ParserExpr::RepMin(_, _)
            | ParserExpr::RepMax(_, _)
            | ParserExpr::RepMinMax(_, _, _) => None,
            ParserExpr::Push(inner) => self.exact(&inner.expr, s)?.push_blank(),
            #[cfg(feature = "grammar-extras")]
            ParserExpr::PushLiteral(string) => {
                if string.is_empty() {
                    s.push_blank()
                } else {
                    None
                }
            }
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.exact(&inner.expr, s),
        }
    }

    fn exact_rule(&mut self, name: &str, s: AbsStack) -> Option<AbsStack> {
        let key = (name.to_owned(), s);
        if let Some(&memo) = self.exact_memo.get(&key) {
            return memo;
        }
        if !self.exact_active.insert(key.clone()) {
            // recursion: not certain
            return None;
        }
        let node = self.rules[name];
        let result = self.exact(&node.expr, s);
        let _ = self.exact_active.remove(&key);
        let _ = self.exact_memo.insert(key, result);
        result
    }
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
                                "rule {} is left-recursive ({}); pest::pratt_parser might be useful \
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
                if is_non_failing(&lhs.expr, rules, &mut vec![trace.last().unwrap().clone()])
                    || is_non_progressing(
                        &lhs.expr,
                        rules,
                        &mut vec![trace.last().unwrap().clone()],
                    )
                {
                    // `lhs` can succeed without consuming input, so both `lhs`
                    // and `rhs` are tried at the current position.
                    check_expr(lhs, rules, trace).or_else(|| check_expr(rhs, rules, trace))
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
            ParserExpr::RepExact(ref node, _)
            | ParserExpr::RepMin(ref node, _)
            | ParserExpr::RepMax(ref node, _)
            | ParserExpr::RepMinMax(ref node, ..) => check_expr(node, rules, trace),
            ParserExpr::PosPred(ref node) => check_expr(node, rules, trace),
            ParserExpr::NegPred(ref node) => check_expr(node, rules, trace),
            ParserExpr::Push(ref node) => check_expr(node, rules, trace),
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(ref node, _) => check_expr(node, rules, trace),
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
    fn non_progressing_empty_string() {
        assert!(is_non_failing(
            &ParserExpr::Insens("".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::Str("".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn progressing_non_empty_string() {
        assert!(!is_non_progressing(
            &ParserExpr::Insens("non empty".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &ParserExpr::Str("non empty".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_progressing_soi_eoi() {
        assert!(is_non_progressing(
            &ParserExpr::Ident("SOI".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::Ident("EOI".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_progressing_predicates() {
        let progressing = ParserExpr::Str("A".into());

        assert!(is_non_progressing(
            &ParserExpr::PosPred(Box::new(ParserNode {
                expr: progressing.clone(),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::NegPred(Box::new(ParserNode {
                expr: progressing,
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_progressing_0_length_repetitions() {
        let input_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("A".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(!is_non_progressing(
            &input_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(is_non_progressing(
            &ParserExpr::Rep(input_progressing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::Opt(input_progressing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::RepExact(input_progressing_node.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::RepMin(input_progressing_node.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::RepMax(input_progressing_node.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::RepMax(input_progressing_node.clone(), 17),
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(is_non_progressing(
            &ParserExpr::RepMinMax(input_progressing_node.clone(), 0, 12),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_progressing_nonzero_repetitions_with_non_progressing_expr() {
        let a = "";
        let non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str(a.into()),
            span: Span::new(a, 0, 0).unwrap(),
        });
        let exact = ParserExpr::RepExact(non_progressing_node.clone(), 7);
        let min = ParserExpr::RepMin(non_progressing_node.clone(), 23);
        let minmax = ParserExpr::RepMinMax(non_progressing_node.clone(), 12, 13);
        let reponce = ParserExpr::RepOnce(non_progressing_node);

        assert!(is_non_progressing(&exact, &HashMap::new(), &mut Vec::new()));
        assert!(is_non_progressing(&min, &HashMap::new(), &mut Vec::new()));
        assert!(is_non_progressing(
            &minmax,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &reponce,
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn progressing_repetitions() {
        let a = "A";
        let input_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str(a.into()),
            span: Span::new(a, 0, 1).unwrap(),
        });
        let exact = ParserExpr::RepExact(input_progressing_node.clone(), 1);
        let min = ParserExpr::RepMin(input_progressing_node.clone(), 2);
        let minmax = ParserExpr::RepMinMax(input_progressing_node.clone(), 4, 5);
        let reponce = ParserExpr::RepOnce(input_progressing_node);

        assert!(!is_non_progressing(
            &exact,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(&min, &HashMap::new(), &mut Vec::new()));
        assert!(!is_non_progressing(
            &minmax,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &reponce,
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_progressing_push() {
        let a = "";
        let non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str(a.into()),
            span: Span::new(a, 0, 0).unwrap(),
        });
        let push = ParserExpr::Push(non_progressing_node.clone());

        assert!(is_non_progressing(&push, &HashMap::new(), &mut Vec::new()));
    }

    #[test]
    fn progressing_push() {
        let a = "i'm make progress";
        let progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str(a.into()),
            span: Span::new(a, 0, 1).unwrap(),
        });
        let push = ParserExpr::Push(progressing_node.clone());

        assert!(!is_non_progressing(&push, &HashMap::new(), &mut Vec::new()));
    }

    #[cfg(feature = "grammar-extras")]
    #[test]
    fn push_literal_is_non_progressing() {
        let a = "";
        let non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::PushLiteral("a".to_string()),
            span: Span::new(a, 0, 0).unwrap(),
        });
        let push = ParserExpr::Push(non_progressing_node.clone());

        assert!(is_non_progressing(&push, &HashMap::new(), &mut Vec::new()));
    }

    #[test]
    fn node_tag_forwards_is_non_progressing() {
        let progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm make progress".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_progressing(
            &progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        let non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(is_non_progressing(
            &non_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        #[cfg(feature = "grammar-extras")]
        {
            let progressing = ParserExpr::NodeTag(progressing_node.clone(), "TAG".into());
            let non_progressing = ParserExpr::NodeTag(non_progressing_node.clone(), "TAG".into());

            assert!(!is_non_progressing(
                &progressing,
                &HashMap::new(),
                &mut Vec::new()
            ));
            assert!(is_non_progressing(
                &non_progressing,
                &HashMap::new(),
                &mut Vec::new()
            ));
        }
    }

    #[test]
    fn progressing_range() {
        let progressing = ParserExpr::Range("A".into(), "Z".into());
        let failing_is_progressing = ParserExpr::Range("Z".into(), "A".into());

        assert!(!is_non_progressing(
            &progressing,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &failing_is_progressing,
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn progressing_choice() {
        let left_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm make progress".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_progressing(
            &left_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_progressing(
            &ParserExpr::Choice(left_progressing_node, right_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_progressing_choices() {
        let left_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm make progress".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(!is_non_progressing(
            &left_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let left_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_progressing(
            &left_non_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_progressing(
            &right_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_progressing(
            &right_non_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(is_non_progressing(
            &ParserExpr::Choice(left_non_progressing_node.clone(), right_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::Choice(left_progressing_node, right_non_progressing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_progressing(
            &ParserExpr::Choice(left_non_progressing_node, right_non_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_progressing_seq() {
        let left_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        let right_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_progressing(
            &ParserExpr::Seq(left_non_progressing_node, right_non_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn progressing_seqs() {
        let left_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm make progress".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(!is_non_progressing(
            &left_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let left_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_progressing(
            &left_non_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_progressing(
            &right_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_non_progressing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_progressing(
            &right_non_progressing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(!is_non_progressing(
            &ParserExpr::Seq(left_non_progressing_node, right_progressing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &ParserExpr::Seq(left_progressing_node.clone(), right_non_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &ParserExpr::Seq(left_progressing_node, right_progressing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn progressing_stack_operations() {
        assert!(!is_non_progressing(
            &ParserExpr::Ident("DROP".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &ParserExpr::Ident("PEEK".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_progressing(
            &ParserExpr::Ident("POP".into()),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_failing_string() {
        let insens = ParserExpr::Insens("".into());
        let string = ParserExpr::Str("".into());

        assert!(is_non_failing(&insens, &HashMap::new(), &mut Vec::new()));

        assert!(is_non_failing(&string, &HashMap::new(), &mut Vec::new()))
    }

    #[test]
    fn failing_string() {
        assert!(!is_non_failing(
            &ParserExpr::Insens("i may fail!".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::Str("failure is not fatal".into()),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn failing_stack_operations() {
        assert!(!is_non_failing(
            &ParserExpr::Ident("DROP".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::Ident("POP".into()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::Ident("PEEK".into()),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_failing_zero_length_repetitions() {
        let failing = Box::new(ParserNode {
            expr: ParserExpr::Range("A".into(), "B".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_failing(
            &failing.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::Opt(failing.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::Rep(failing.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepExact(failing.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMin(failing.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMax(failing.clone(), 0),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMax(failing.clone(), 22),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMinMax(failing.clone(), 0, 73),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn non_failing_non_zero_repetitions_with_non_failing_expr() {
        let non_failing = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Range("A".into(), "B".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(is_non_failing(
            &non_failing.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepOnce(non_failing.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepExact(non_failing.clone(), 1),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMin(non_failing.clone(), 6),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::RepMinMax(non_failing.clone(), 32, 73),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    #[cfg(feature = "grammar-extras")]
    fn failing_non_zero_repetitions() {
        let failing = Box::new(ParserNode {
            expr: ParserExpr::NodeTag(
                Box::new(ParserNode {
                    expr: ParserExpr::Range("A".into(), "B".into()),
                    span: Span::new(" ", 0, 1).unwrap(),
                }),
                "Tag".into(),
            ),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_failing(
            &failing.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::RepOnce(failing.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::RepExact(failing.clone(), 3),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::RepMin(failing.clone(), 14),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::RepMinMax(failing.clone(), 47, 73),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn failing_choice() {
        let left_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm a failure".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_failing(
            &left_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_failing(
            &ParserExpr::Choice(left_failing_node, right_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_failing_choices() {
        let left_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm a failure".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(!is_non_failing(
            &left_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let left_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_failing(
            &left_non_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_failing(
            &right_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_failing(
            &right_non_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(is_non_failing(
            &ParserExpr::Choice(left_non_failing_node.clone(), right_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::Choice(left_failing_node, right_non_failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::Choice(left_non_failing_node, right_non_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn non_failing_seq() {
        let left_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        let right_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_failing(
            &ParserExpr::Seq(left_non_failing_node, right_non_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn failing_seqs() {
        let left_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm a failure".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(!is_non_failing(
            &left_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let left_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_failing(
            &left_non_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Ident("DROP".into()),
            span: Span::new("DROP", 0, 3).unwrap(),
        });

        assert!(!is_non_failing(
            &right_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        let right_non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Opt(Box::new(ParserNode {
                expr: ParserExpr::Str("   ".into()),
                span: Span::new(" ", 0, 1).unwrap(),
            })),
            span: Span::new(" ", 0, 1).unwrap(),
        });

        assert!(is_non_failing(
            &right_non_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(!is_non_failing(
            &ParserExpr::Seq(left_non_failing_node, right_failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::Seq(left_failing_node.clone(), right_non_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(!is_non_failing(
            &ParserExpr::Seq(left_failing_node, right_failing_node),
            &HashMap::new(),
            &mut Vec::new()
        ))
    }

    #[test]
    fn failing_range() {
        let failing = ParserExpr::Range("A".into(), "Z".into());
        let always_failing = ParserExpr::Range("Z".into(), "A".into());

        assert!(!is_non_failing(&failing, &HashMap::new(), &mut Vec::new()));
        assert!(!is_non_failing(
            &always_failing,
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[test]
    fn _push_node_tag_pos_pred_forwarding_is_non_failing() {
        let failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("i'm a failure".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(!is_non_failing(
            &failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));
        let non_failing_node = Box::new(ParserNode {
            expr: ParserExpr::Str("".into()),
            span: Span::new(" ", 0, 1).unwrap(),
        });
        assert!(is_non_failing(
            &non_failing_node.clone().expr,
            &HashMap::new(),
            &mut Vec::new()
        ));

        #[cfg(feature = "grammar-extras")]
        {
            assert!(!is_non_failing(
                &ParserExpr::NodeTag(failing_node.clone(), "TAG".into()),
                &HashMap::new(),
                &mut Vec::new()
            ));
            assert!(is_non_failing(
                &ParserExpr::NodeTag(non_failing_node.clone(), "TAG".into()),
                &HashMap::new(),
                &mut Vec::new()
            ));
        }

        assert!(!is_non_failing(
            &ParserExpr::Push(failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::Push(non_failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));

        assert!(!is_non_failing(
            &ParserExpr::PosPred(failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
        assert!(is_non_failing(
            &ParserExpr::PosPred(non_failing_node.clone()),
            &HashMap::new(),
            &mut Vec::new()
        ));
    }

    #[cfg(feature = "grammar-extras")]
    #[test]
    fn push_literal_is_non_failing() {
        assert!(is_non_failing(
            &ParserExpr::PushLiteral("a".to_string()),
            &HashMap::new(),
            &mut Vec::new()
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
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
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
  = rule b is left-recursive (b -> a -> b); pest::pratt_parser might be useful in this case

 --> 1:17
  |
1 | a = { b } b = { a }
  |                 ^
  |
  = rule a is left-recursive (a -> b -> a); pest::pratt_parser might be useful in this case")]
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
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
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
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
    fn non_primary_choice_left_recursion() {
        let input = "a = { \"a\" | a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:14
  |
1 | a = { !\"a\" ~ a }
  |              ^
  |
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
    fn non_progressing_left_recursion() {
        let input = "a = { !\"a\" ~ a }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:8
  |
1 | a = { (a | \"\") ~ \".\" }
  |        ^
  |
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
    fn non_failing_lhs_left_recursion() {
        let input = "a = { (a | \"\") ~ \".\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { a? ~ \"x\" }
  |       ^
  |
  = rule a is left-recursive (a -> a); pest::pratt_parser might be useful in this case")]
    fn optional_lhs_left_recursion() {
        let input = "a = { a? ~ \"x\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { b* ~ \"x\" } b = { a ~ \"y\" }
  |       ^
  |
  = rule b is left-recursive (b -> a -> b); pest::pratt_parser might be useful in this case

 --> 1:24
  |
1 | a = { b* ~ \"x\" } b = { a ~ \"y\" }
  |                        ^
  |
  = rule a is left-recursive (a -> b -> a); pest::pratt_parser might be useful in this case")]
    fn indirect_repeated_lhs_left_recursion() {
        let input = "a = { b* ~ \"x\" } b = { a ~ \"y\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | r = { r{,1} ~ \"x\" }
  |       ^
  |
  = rule r is left-recursive (r -> r); pest::pratt_parser might be useful in this case")]
    fn bounded_repeat_lhs_left_recursion() {
        let input = "r = { r{,1} ~ \"x\" }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    fn bounded_repeat_lhs_left_recursion_all_forms() {
        for input in [
            "r = { r{,1} ~ \"x\" }",
            "r = { r{0,} ~ \"x\" }",
            "r = { r{0, 2} ~ \"x\" }",
            "r = { r{2} ~ \"x\" }",
            "r = { r{1,} ~ \"x\" }",
            "r = { r{1, 2} ~ \"x\" }",
            "r = { (\"a\"{,1} ~ r) ~ \"x\" }",
        ] {
            let errors = consume_rules(PestParser::parse(Rule::grammar_rules, input).unwrap())
                .expect_err(input);
            assert!(
                errors
                    .iter()
                    .any(|e| e.to_string().contains("rule r is left-recursive (r -> r)")),
                "{input}: {errors:?}"
            );
        }
    }

    #[cfg(feature = "grammar-extras")]
    #[test]
    fn tagged_lhs_left_recursion() {
        let input = "r = { #t = r? ~ \"x\" }";
        let errors =
            consume_rules(PestParser::parse(Rule::grammar_rules, input).unwrap()).expect_err(input);
        assert!(
            errors
                .iter()
                .any(|e| e.to_string().contains("rule r is left-recursive (r -> r)")),
            "{errors:?}"
        );
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

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { #b = b } b = _{ ASCII_DIGIT+ }
  |       ^----^
  |
  = tags on silent rules will not appear in the output")]
    #[cfg(feature = "grammar-extras")]
    fn tag_on_silent_rule() {
        let input = "a = { #b = b } b = _{ ASCII_DIGIT+ }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { #b = ASCII_DIGIT+ }
  |       ^---------------^
  |
  = tags on built-in rules will not appear in the output")]
    #[cfg(feature = "grammar-extras")]
    fn tag_on_builtin_rule() {
        let input = "a = { #b = ASCII_DIGIT+ }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[cfg(feature = "grammar-extras")]
    fn tag_on_normal_rule() {
        let input = "a = { #b = b } b = { ASCII_DIGIT+ }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    fn stack_loop_errors(input: &str) -> Vec<String> {
        match consume_rules(PestParser::parse(Rule::grammar_rules, input).unwrap()) {
            Ok(_) => vec![],
            Err(errors) => errors
                .iter()
                .map(|e| format!("{:?} {}", e.line_col, e.variant.message()))
                .collect(),
        }
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:17
  |
1 | a = { POP_ALL ~ PEEK_ALL* }
  |                 ^-------^
  |
  = expression inside repetition is non-progressing and will repeat infinitely")]
    fn peek_all_repetition_after_pop_all() {
        let input = "a = { POP_ALL ~ PEEK_ALL* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    #[should_panic(expected = "grammar error

 --> 1:7
  |
1 | a = { (PUSH(\"\") ~ POP)* }
  |       ^---------------^
  |
  = expression inside repetition is non-progressing and will repeat infinitely")]
    fn push_empty_then_pop_repetition() {
        let input = "a = { (PUSH(\"\") ~ POP)* }";
        unwrap_or_report(consume_rules(
            PestParser::parse(Rule::grammar_rules, input).unwrap(),
        ));
    }

    #[test]
    fn stack_repetitions_that_loop() {
        for input in [
            // after POP_ALL the stack is empty, so these match nothing and never fail
            "a = { POP_ALL ~ PEEK_ALL* }",
            "a = { POP_ALL ~ POP_ALL+ }",
            "a = { POP_ALL ~ PEEK[..]* }",
            "a = { POP_ALL ~ (PEEK_ALL ~ POP_ALL){2,} }",
            // through a rule
            "e = { PEEK_ALL } a = { POP_ALL ~ e* }",
            // PUSH of an empty match then POP leaves everything as it was
            "a = { (PUSH(\"\") ~ POP)* }",
            "a = { (PUSH(\"\") ~ DROP)* }",
            "a = { (PUSH(\"\") ~ PEEK ~ POP)* }",
            "a = { (PUSH(\"\") ~ PUSH(\"\") ~ DROP ~ DROP)* }",
            // the stack is all empty strings after POP_ALL ~ PUSH("")
            "a = { POP_ALL ~ PUSH(\"\") ~ PEEK_ALL* }",
        ] {
            let errors = stack_loop_errors(input);
            assert_eq!(errors.len(), 1, "{input}: {errors:?}");
            assert!(
                errors[0].contains("non-progressing and will repeat infinitely"),
                "{input}: {errors:?}"
            );
        }
    }

    #[test]
    fn stack_repetitions_that_may_end() {
        for input in [
            // terminates when the stack holds a non-empty string, e.g. after PUSH("x") ~ a
            "a = { PEEK_ALL* }",
            "a = { POP_ALL* }",
            "a = { PEEK[..]* }",
            "a = { PEEK_ALL* } b = { PUSH(\"x\") ~ a }",
            // terminates on non-empty input
            "a = { PUSH(ANY*) ~ PEEK_ALL* }",
            "a = { POP_ALL ~ PUSH(ASCII_DIGIT+) ~ PEEK_ALL* }",
            // the stack only grows
            "a = { (PUSH(\"\") ~ PEEK)* }",
            // DROP eventually fails
            "a = { DROP* }",
            "a = { POP_ALL ~ (PUSH(\"\") ~ DROP ~ DROP)* }",
            // a slice that is out of range fails
            "a = { POP_ALL ~ PEEK[0..1]* }",
            // the body consumes input
            "a = { POP_ALL ~ (PEEK_ALL ~ \"x\")* }",
            "a = { (PUSH(\"x\") ~ POP)* }",
            // a negative predicate may fail
            "a = { POP_ALL ~ (!\"x\" ~ PEEK_ALL)* }",
            // POP or PEEK on an empty stack panic rather than loop
            "a = { POP_ALL ~ PEEK* }",
        ] {
            assert_eq!(stack_loop_errors(input), Vec::<String>::new(), "{input}");
        }
    }

    #[test]
    fn stack_repetition_is_not_reported_when_whitespace_changes_the_stack() {
        let input = "WHITESPACE = _{ PUSH(\" \") } a = { POP_ALL ~ PEEK_ALL* }";
        assert_eq!(stack_loop_errors(input), Vec::<String>::new());
    }

    #[test]
    fn stack_repetition_errors_once() {
        // the existing check already reports these; the stack check does not repeat them
        for input in ["a = { \"\"* }", "a = { (POP_ALL ~ \"\")* }"] {
            let errors = stack_loop_errors(input);
            assert!(errors.len() <= 1, "{input}: {errors:?}");
        }
        assert_eq!(stack_loop_errors("a = { \"\"* }").len(), 1);
    }
}
