// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Repetitions that never end because of the stack.
//!
//! `is_non_progressing` and `is_non_failing` treat the stack builtins as progressing, which
//! misses for example `POP_ALL ~ PEEK_ALL*` (`PEEK_ALL` matches nothing on an empty stack) and
//! `(PUSH("") ~ POP)*`. This pass tracks what is known about the stack through each rule and
//! reports a repetition whose body, from every stack that can reach it, succeeds without
//! consuming input and within a few iterations leaves the stack as it found it.
//!
//! It only reports what is certain, so a grammar that can terminate is never rejected:
//! - every rule is analysed as if entered with an unknown stack, so a bare `PEEK_ALL*` is not
//!   reported (it terminates when the stack holds a non-empty string);
//! - repetitions that cannot be reached are not reported (after a choice alternative that
//!   always succeeds, or after an expression that never succeeds);
//! - a sequence is not known to consume nothing when `WHITESPACE` or `COMMENT` is defined,
//!   since implicit whitespace may run between its operands;
//! - if `WHITESPACE` or `COMMENT` can change the stack, nothing is reported.

use std::collections::{HashMap, HashSet};

use pest::error::{Error, ErrorVariant};

use super::{is_non_failing, is_non_progressing, to_hash_map};
use crate::parser::{ParserExpr, ParserNode, ParserRule, Rule};

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

    /// Certainly no entries.
    fn is_empty(self) -> bool {
        self.base == StackBase::Empty && self.top == 0
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

/// Joins two possibly unreachable states (`None` = the point cannot be reached).
fn join(a: Option<AbsStack>, b: Option<AbsStack>) -> Option<AbsStack> {
    match (a, b) {
        (Some(a), Some(b)) => Some(a.join(b)),
        (a, None) => a,
        (None, b) => b,
    }
}

pub(super) fn validate_stack_repetition<'a, 'i: 'a>(
    rules: &'a [ParserRule<'i>],
) -> Vec<Error<Rule>> {
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
        skips: map.contains_key("WHITESPACE") || map.contains_key("COMMENT"),
        post_memo: HashMap::new(),
        exact_memo: HashMap::new(),
        never_memo: HashMap::new(),
        post_active: HashSet::new(),
        exact_active: HashSet::new(),
        never_active: HashSet::new(),
    };
    let mut errors = vec![];
    let entry = AbsStack {
        base: StackBase::Any,
        top: 0,
        base_intact: true,
    };
    for rule in rules {
        analysis.walk(&rule.node, Some(entry), &mut errors);
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

type Memo<T> = HashMap<(String, AbsStack), T>;

struct StackAnalysis<'a, 'i> {
    rules: &'a HashMap<String, &'a ParserNode<'i>>,
    /// `WHITESPACE` or `COMMENT` is defined, so implicit whitespace may be consumed between
    /// the operands of a sequence (in non-atomic rules, which are not tracked).
    skips: bool,
    post_memo: Memo<AbsStack>,
    exact_memo: Memo<Option<AbsStack>>,
    never_memo: Memo<bool>,
    post_active: HashSet<(String, AbsStack)>,
    exact_active: HashSet<(String, AbsStack)>,
    never_active: HashSet<(String, AbsStack)>,
}

impl<'i> StackAnalysis<'_, 'i> {
    /// Visits `node` entered with stack `s` (`None`: it cannot be reached), reports looping
    /// repetitions in it, and returns the stack after `node` succeeds.
    fn walk(
        &mut self,
        node: &ParserNode<'i>,
        s: Option<AbsStack>,
        errors: &mut Vec<Error<Rule>>,
    ) -> Option<AbsStack> {
        let s = s?;
        match &node.expr {
            ParserExpr::Seq(lhs, rhs) => {
                let mid = self.walk(lhs, Some(s), errors);
                let mid = if self.never_succeeds(&lhs.expr, s) {
                    None
                } else {
                    mid
                };
                self.walk(rhs, mid, errors)
            }
            ParserExpr::Choice(lhs, rhs) => {
                let l = self.walk(lhs, Some(s), errors);
                // `rhs` is only tried when `lhs` fails.
                let lhs_always_succeeds = self.exact(&lhs.expr, s).is_some()
                    || is_non_failing(&lhs.expr, self.rules, &mut vec![]);
                let r = if lhs_always_succeeds {
                    None
                } else {
                    self.walk(rhs, Some(s), errors)
                };
                join(l, r)
            }
            ParserExpr::Opt(inner) => join(Some(s), self.walk(inner, Some(s), errors)),
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
                self.walk(inner, Some(every), errors);
                Some(every)
            }
            ParserExpr::RepExact(inner, _)
            | ParserExpr::RepMax(inner, _)
            | ParserExpr::RepMinMax(inner, _, _) => {
                let every = self.post_repeated(&inner.expr, s);
                self.walk(inner, Some(every), errors);
                Some(every)
            }
            ParserExpr::PosPred(inner) | ParserExpr::NegPred(inner) => {
                self.walk(inner, Some(s), errors);
                Some(s)
            }
            ParserExpr::Push(inner) => {
                self.walk(inner, Some(s), errors);
                Some(self.post(&node.expr, s))
            }
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.walk(inner, Some(s), errors),
            _ => Some(self.post(&node.expr, s)),
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
            // Implicit whitespace may be consumed between the operands.
            ParserExpr::Seq(_, _) if self.skips => None,
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

    /// From every stack `s` allows and on any input, `expr` never succeeds: it fails, or
    /// panics (`POP` or `PEEK` on an empty stack). What follows it in a sequence cannot run.
    fn never_succeeds(&mut self, expr: &ParserExpr<'i>, s: AbsStack) -> bool {
        match expr {
            ParserExpr::Ident(name) => match name.as_str() {
                "DROP" | "POP" | "PEEK" => s.is_empty(),
                _ => self.rules.contains_key(name) && self.never_succeeds_rule(name, s),
            },
            ParserExpr::Seq(lhs, rhs) => {
                self.never_succeeds(&lhs.expr, s) || {
                    let mid = self.post(&lhs.expr, s);
                    self.never_succeeds(&rhs.expr, mid)
                }
            }
            ParserExpr::Choice(lhs, rhs) => {
                self.never_succeeds(&lhs.expr, s) && self.never_succeeds(&rhs.expr, s)
            }
            ParserExpr::RepOnce(inner)
            | ParserExpr::RepExact(inner, 1..)
            | ParserExpr::RepMin(inner, 1..)
            | ParserExpr::RepMinMax(inner, 1.., _)
            | ParserExpr::PosPred(inner)
            | ParserExpr::Push(inner) => self.never_succeeds(&inner.expr, s),
            ParserExpr::NegPred(inner) => self.exact(&inner.expr, s).is_some(),
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.never_succeeds(&inner.expr, s),
            _ => false,
        }
    }

    fn never_succeeds_rule(&mut self, name: &str, s: AbsStack) -> bool {
        let key = (name.to_owned(), s);
        if let Some(&memo) = self.never_memo.get(&key) {
            return memo;
        }
        if !self.never_active.insert(key.clone()) {
            // recursion: not certain
            return false;
        }
        let node = self.rules[name];
        let result = self.never_succeeds(&node.expr, s);
        let _ = self.never_active.remove(&key);
        let _ = self.never_memo.insert(key, result);
        result
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::parser::{consume_rules, PestParser, Rule};
    use crate::unwrap_or_report;

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
            // DROP fails on the empty stack, so the second alternative is tried
            "a = { POP_ALL ~ (DROP | PEEK_ALL*) }",
            // implicit whitespace between iterations only delays the loop
            "WHITESPACE = _{ \" \" } a = { POP_ALL ~ PEEK_ALL* }",
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
            // the repetition is never reached: the first alternative always succeeds
            "a = { POP_ALL ~ (PEEK_ALL | PEEK_ALL*) }",
            // the repetition is never reached: DROP fails on the empty stack
            "a = { POP_ALL ~ (DROP ~ PEEK_ALL*)? }",
            // implicit whitespace inside the PUSH is captured, so POP fails at the end
            "WHITESPACE = _{ \" \" } a = { (PUSH(\"\" ~ \"\") ~ POP)* }",
            // the same through a non-atomic rule called from an atomic one
            "WHITESPACE = _{ \" \" } b = !{ \"\" ~ \"\" } a = @{ POP_ALL ~ PUSH(b) ~ PEEK_ALL* }",
        ] {
            assert_eq!(stack_loop_errors(input), Vec::<String>::new(), "{input}");
        }
    }

    /// Loops this pass does not detect. They are listed so that detecting them later shows up
    /// as an intended change, not a regression.
    #[test]
    fn stack_repetitions_that_loop_but_are_not_detected() {
        for input in [
            // the stack grows by one empty string per iteration, so it never returns to the
            // state an iteration started from
            "a = { (PUSH(\"\") ~ PEEK)* }",
            // a sequence is not known to consume nothing when implicit whitespace exists
            "WHITESPACE = _{ \" \" } a = { (PUSH(\"\") ~ POP)* }",
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
