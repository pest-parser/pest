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
//! - each expression yields the stack after it succeeds and the stack after it fails, each
//!   possibly "cannot happen": a repetition is only analysed from stacks that can reach it,
//!   and a choice's right side from the stack its left side fails with (restored when the
//!   optimizer wraps the branch in `RestoreOnErr`; a bare failed `POP_ALL` keeps what it
//!   removed);
//! - bounded repetitions are followed as the optimizer unrolls them, copy by copy, never past
//!   their bound (the stacks repeat with some period, so huge bounds are cheap);
//! - a sequence or a repetition is not known to consume nothing when `WHITESPACE` or
//!   `COMMENT` is defined, since implicit whitespace may run between its parts;
//! - if `WHITESPACE` or `COMMENT` can change the stack, nothing is reported.
//!
//! Results are cached per expression node and stack, so each node is analysed once per stack.

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

/// What is known about the stack at a point of a rule: an unknown `base` with `len` entries
/// pushed on it, each known to be an empty string or not.
///
/// Empty strings matter because they are the only entries that `PEEK`, `POP`, `PEEK_ALL` and
/// `POP_ALL` can match without consuming input; the other entries are tracked so that `DROP`,
/// `POP` and `PEEK` are known to find something to remove or match.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct AbsStack {
    base: StackBase,
    /// Entries pushed on `base`.
    len: u8,
    /// Bit `i` set: entry `i` (from the bottom of the pushed ones) is certainly an empty string.
    blank: u8,
    /// `base` still holds what it held when the current repetition iteration started.
    base_intact: bool,
}

impl AbsStack {
    /// Most entries tracked on top of the base; beyond that the state is given up.
    const MAX_LEN: u8 = 8;

    const UNKNOWN: AbsStack = AbsStack {
        base: StackBase::Any,
        len: 0,
        blank: 0,
        base_intact: false,
    };

    fn mask(len: u8) -> u8 {
        ((1u16 << len) - 1) as u8
    }

    fn join(self, other: AbsStack) -> AbsStack {
        if self == other {
            return self;
        }
        let base = match (self.base, other.base) {
            (a, b) if a == b => a,
            (StackBase::Any, _) | (_, StackBase::Any) => StackBase::Any,
            // one base empty and the other all blank (in either order): all blank
            _ => StackBase::Blank,
        };
        if self.len == other.len {
            AbsStack {
                base,
                len: self.len,
                // an entry is known blank only if it is in both
                blank: self.blank & other.blank,
                base_intact: self.base_intact && other.base_intact,
            }
        } else if base != StackBase::Any && self.all_blank() && other.all_blank() {
            // different numbers of entries, all empty strings
            AbsStack {
                base: StackBase::Blank,
                len: 0,
                blank: 0,
                base_intact: false,
            }
        } else {
            AbsStack::UNKNOWN
        }
    }

    /// The state after pushing an entry that is certainly an empty string (`blank`) or not.
    fn push(self, blank: bool) -> AbsStack {
        if self.len < AbsStack::MAX_LEN {
            AbsStack {
                len: self.len + 1,
                blank: self.blank | (u8::from(blank) << self.len),
                ..self
            }
        } else {
            AbsStack::UNKNOWN
        }
    }

    /// Every entry is an empty string.
    fn all_blank(self) -> bool {
        self.base != StackBase::Any && self.blank == AbsStack::mask(self.len)
    }

    /// Certainly no entries.
    fn is_empty(self) -> bool {
        self.base == StackBase::Empty && self.len == 0
    }

    /// Certainly at least one entry.
    fn non_empty(self) -> bool {
        self.len > 0
    }

    /// The top entry is certainly an empty string.
    fn top_blank(self) -> bool {
        self.len > 0 && self.blank >> (self.len - 1) & 1 == 1
    }

    /// The state after `POP_ALL` succeeds.
    fn popped_all(self) -> AbsStack {
        AbsStack {
            base: StackBase::Empty,
            len: 0,
            blank: 0,
            base_intact: self.base_intact && self.base == StackBase::Empty,
        }
    }

    /// The state after one entry is removed (`POP`, `DROP`).
    fn popped_one(self) -> AbsStack {
        if self.len > 0 {
            AbsStack {
                len: self.len - 1,
                blank: self.blank & AbsStack::mask(self.len - 1),
                ..self
            }
        } else {
            match self.base {
                StackBase::Empty => self,
                StackBase::Blank => AbsStack {
                    base: StackBase::Blank,
                    len: 0,
                    blank: 0,
                    base_intact: false,
                },
                StackBase::Any => AbsStack::UNKNOWN,
            }
        }
    }

    /// The same pushed entries as `other` (the base is compared through `base_intact`).
    fn same_top(self, other: AbsStack) -> bool {
        self.len == other.len && self.blank == other.blank
    }
}

/// A possibly impossible state (`None`: that outcome cannot happen).
type Maybe = Option<AbsStack>;

/// Joins two possibly impossible states.
fn join(a: Maybe, b: Maybe) -> Maybe {
    match (a, b) {
        (Some(a), Some(b)) => Some(a.join(b)),
        (a, None) => a,
        (None, b) => b,
    }
}

/// What an expression does when entered with a given stack.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Outcome {
    /// The stack after it succeeds, or `None` if it never succeeds.
    ok: Maybe,
    /// The stack after it fails, or `None` if it never fails.
    err: Maybe,
    /// When it succeeds it consumes no input, whatever the input (only meaningful with `ok`).
    empty: bool,
}

impl Outcome {
    /// Nothing is known except the stack it was entered with, which it may have changed.
    const UNKNOWN: Outcome = Outcome {
        ok: Some(AbsStack::UNKNOWN),
        err: Some(AbsStack::UNKNOWN),
        empty: false,
    };

    fn succeeds(s: AbsStack, empty: bool) -> Outcome {
        Outcome {
            ok: Some(s),
            err: None,
            empty,
        }
    }

    fn fails(s: AbsStack) -> Outcome {
        Outcome {
            ok: None,
            err: Some(s),
            empty: false,
        }
    }

    /// May succeed (consuming input, or not) or fail, leaving the stack as `s`.
    fn either(s: AbsStack) -> Outcome {
        Outcome {
            ok: Some(s),
            err: Some(s),
            empty: false,
        }
    }

    /// Certainly succeeds without consuming input, whatever the input: `Some(stack after)`.
    fn exact(self) -> Maybe {
        if self.err.is_none() && self.empty {
            self.ok
        } else {
            None
        }
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
        memo: HashMap::new(),
        active: HashSet::new(),
        rule_memo: HashMap::new(),
        rule_active: HashSet::new(),
        errors: vec![],
        reported: HashSet::new(),
        walking: false,
        walked: HashSet::new(),
    };
    let entry = AbsStack {
        base: StackBase::Any,
        len: 0,
        blank: 0,
        base_intact: true,
    };
    for rule in rules {
        analysis.walk(&rule.node, entry);
    }
    analysis.errors
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
    /// `WHITESPACE` or `COMMENT` is defined, so implicit whitespace may be consumed between
    /// the operands of a sequence (in non-atomic rules, which are not tracked).
    skips: bool,
    /// Outcome of an expression node (by address) entered with a stack.
    memo: HashMap<(*const ParserExpr<'i>, AbsStack), Outcome>,
    active: HashSet<(*const ParserExpr<'i>, AbsStack)>,
    /// Outcome of a rule entered with a stack.
    rule_memo: HashMap<(String, AbsStack), Outcome>,
    rule_active: HashSet<(String, AbsStack)>,
    errors: Vec<Error<Rule>>,
    /// Repetition nodes already reported, so one is not reported twice.
    reported: HashSet<*const ParserNode<'i>>,
    /// Inside `walk`: repetitions reached are checked and reported.
    walking: bool,
    /// Nodes already walked with a stack: what they report depends only on the node and the
    /// stack, and is already in `errors`, so they are not walked again.
    walked: HashSet<(*const ParserExpr<'i>, AbsStack)>,
}

impl<'a, 'i> StackAnalysis<'a, 'i> {
    /// Analyses a rule's body entered with `s`, reporting the looping repetitions reached.
    fn walk(&mut self, node: &'a ParserNode<'i>, s: AbsStack) {
        self.walking = true;
        let _ = self.visit(node, s);
        self.walking = false;
    }

    /// The outcome of `node` entered with `s`; while walking, also reports the looping
    /// repetitions it reaches. Each node is walked once per stack.
    fn visit(&mut self, node: &'a ParserNode<'i>, s: AbsStack) -> Outcome {
        if self.walking && self.walked.insert((&node.expr as *const ParserExpr<'i>, s)) {
            self.step(node, s)
        } else {
            self.outcome(node, s)
        }
    }

    /// The outcome of `node` entered with `s`, cached; never reports.
    fn outcome(&mut self, node: &'a ParserNode<'i>, s: AbsStack) -> Outcome {
        let key = (&node.expr as *const ParserExpr<'i>, s);
        if let Some(&o) = self.memo.get(&key) {
            return o;
        }
        if !self.active.insert(key) {
            return Outcome::UNKNOWN;
        }
        let walking = std::mem::replace(&mut self.walking, false);
        let o = self.step(node, s);
        self.walking = walking;
        let _ = self.active.remove(&key);
        let _ = self.memo.insert(key, o);
        o
    }

    fn step(&mut self, node: &'a ParserNode<'i>, s: AbsStack) -> Outcome {
        match &node.expr {
            ParserExpr::Str(string) | ParserExpr::Insens(string) => {
                if string.is_empty() {
                    Outcome::succeeds(s, true)
                } else {
                    Outcome::either(s)
                }
            }
            ParserExpr::Range(_, _) => Outcome::either(s),
            ParserExpr::Ident(name) => self.ident(name, s),
            // `PEEK[..]` is `PEEK_ALL` matched bottom to top. `PEEK[0..0]` is always in range
            // (0 is never past the end) and matches nothing. Other slices can be out of range
            // or consume input.
            ParserExpr::PeekSlice(0, None) => self.ident("PEEK_ALL", s),
            ParserExpr::PeekSlice(0, Some(0)) => Outcome::succeeds(s, true),
            ParserExpr::PeekSlice(_, _) => Outcome::either(s),
            ParserExpr::PosPred(inner) => {
                let o = self.visit(inner, s);
                Outcome {
                    ok: o.ok.map(|_| s),
                    err: o.err.map(|_| s),
                    empty: true,
                }
            }
            ParserExpr::NegPred(inner) => {
                let o = self.visit(inner, s);
                Outcome {
                    ok: o.err.map(|_| s),
                    err: o.ok.map(|_| s),
                    empty: true,
                }
            }
            ParserExpr::Seq(lhs, rhs) => {
                let l = self.visit(lhs, s);
                let r = match l.ok {
                    Some(mid) => self.visit(rhs, mid),
                    None => Outcome {
                        ok: None,
                        err: None,
                        empty: true,
                    },
                };
                Outcome {
                    ok: r.ok,
                    // a failed sequence restores the stack (`state.sequence`)
                    err: if l.err.is_some() || r.err.is_some() {
                        Some(s)
                    } else {
                        None
                    },
                    // implicit whitespace may be consumed between the operands
                    empty: l.empty && r.empty && !self.skips,
                }
            }
            ParserExpr::Choice(lhs, rhs) => {
                let l = self.visit(lhs, s);
                // `rhs` is only tried when `lhs` fails, from the stack `lhs` failed with.
                let r = match self.failed(l.err, s) {
                    Some(failed) => self.visit(rhs, failed),
                    None => Outcome {
                        ok: None,
                        err: None,
                        empty: true,
                    },
                };
                Outcome {
                    ok: join(l.ok, r.ok),
                    err: self.failed(r.err, s),
                    empty: (l.ok.is_none() || l.empty) && (r.ok.is_none() || r.empty),
                }
            }
            ParserExpr::Opt(inner) => {
                let o = self.visit(inner, s);
                Outcome {
                    ok: join(o.ok, self.failed(o.err, s)),
                    err: None,
                    empty: o.ok.is_none() || o.empty,
                }
            }
            ParserExpr::Rep(inner) => self.repeat(node, inner, s, 0, None),
            ParserExpr::RepOnce(inner) => self.repeat(node, inner, s, 1, None),
            ParserExpr::RepMin(inner, min) => self.repeat(node, inner, s, *min, None),
            ParserExpr::RepExact(inner, n) => self.repeat(node, inner, s, *n, Some(*n)),
            ParserExpr::RepMax(inner, max) => self.repeat(node, inner, s, 0, Some(*max)),
            // `{min, max}` with `min > max` is accepted, and the optimizer then builds `max`
            // required copies (`unroller.rs`)
            ParserExpr::RepMinMax(inner, min, max) => {
                self.repeat(node, inner, s, (*min).min(*max), Some(*max))
            }
            ParserExpr::Push(inner) => {
                let o = self.visit(inner, s);
                Outcome {
                    // pushes what it matched: an empty string if it certainly matched nothing
                    ok: o.ok.map(|after| after.push(o.empty)),
                    err: o.err,
                    empty: o.empty,
                }
            }
            #[cfg(feature = "grammar-extras")]
            ParserExpr::PushLiteral(string) => Outcome::succeeds(s.push(string.is_empty()), true),
            #[cfg(feature = "grammar-extras")]
            ParserExpr::NodeTag(inner, _) => self.visit(inner, s),
        }
    }

    fn ident(&mut self, name: &str, s: AbsStack) -> Outcome {
        match name {
            // matches every entry, top to bottom; nothing to match when all are empty strings
            "PEEK_ALL" => {
                if s.all_blank() {
                    Outcome::succeeds(s, true)
                } else {
                    Outcome::either(s)
                }
            }
            "POP_ALL" => {
                if s.all_blank() {
                    Outcome::succeeds(s.popped_all(), true)
                } else {
                    // A failed POP_ALL keeps the entries it removed before the mismatch
                    // (it is not restored), so after a failure only "unknown" is safe.
                    Outcome {
                        ok: Some(s.popped_all()),
                        err: Some(AbsStack::UNKNOWN),
                        empty: false,
                    }
                }
            }
            // On an empty stack POP and PEEK panic and DROP fails: none of them succeeds.
            "PEEK" | "POP" => {
                if s.is_empty() {
                    Outcome {
                        ok: None,
                        err: None,
                        empty: true,
                    }
                } else {
                    let after = if name == "POP" { s.popped_one() } else { s };
                    if s.top_blank() {
                        Outcome::succeeds(after, true)
                    } else {
                        // matches the top entry: may consume input or fail
                        Outcome {
                            ok: Some(after),
                            err: Some(after),
                            empty: false,
                        }
                    }
                }
            }
            "DROP" => {
                if s.is_empty() {
                    Outcome::fails(s)
                } else if s.non_empty() {
                    Outcome::succeeds(s.popped_one(), true)
                } else {
                    Outcome {
                        ok: Some(s.popped_one()),
                        err: Some(s),
                        empty: true,
                    }
                }
            }
            _ => match self.rules.get(name) {
                Some(&node) => self.rule(name, node, s),
                // other builtins (ANY, SOI, EOI, ...) do not touch the stack
                None => Outcome::either(s),
            },
        }
    }

    fn rule(&mut self, name: &str, node: &'a ParserNode<'i>, s: AbsStack) -> Outcome {
        let key = (name.to_owned(), s);
        if let Some(&o) = self.rule_memo.get(&key) {
            return o;
        }
        if !self.rule_active.insert(key.clone()) {
            // recursion: give up
            return Outcome::UNKNOWN;
        }
        let walking = std::mem::replace(&mut self.walking, false);
        let o = self.outcome(node, s);
        self.walking = walking;
        let _ = self.rule_active.remove(&key);
        let _ = self.rule_memo.insert(key, o);
        o
    }

    /// The stack after `inner` failed with `err` (`s`: the stack it started with), as the
    /// alternative or optional around it sees it. The optimizer wraps some branches in
    /// `RestoreOnErr`, which restores `s` on failure, and leaves others as they are, which
    /// keep what a failed `POP_ALL` removed. Which branches are wrapped depends on the
    /// optimized shape of the grammar (choices are rotated, `PUSH_LITERAL` doesn't count), so
    /// both stacks are taken as possible.
    fn failed(&mut self, err: Maybe, s: AbsStack) -> Maybe {
        err.map(|err| err.join(s))
    }

    /// A repetition of `inner` between `min` and `max` times (unbounded if `None`), entered
    /// with `s`, as the optimizer unrolls it (`unroller.rs`): `min` copies of `inner` in
    /// sequence, then `inner*` if unbounded, or `max - min` copies of `inner?`. While walking,
    /// the body is visited from every stack an iteration can start with, and an unbounded
    /// repetition that loops forever is reported.
    fn repeat(
        &mut self,
        node: &'a ParserNode<'i>,
        inner: &'a ParserNode<'i>,
        s: AbsStack,
        min: u32,
        max: Option<u32>,
    ) -> Outcome {
        if max.is_none() && self.walking && self.loops_forever(inner, s) {
            self.report(node, inner);
        }
        // implicit whitespace may be consumed between iterations
        let mut empty = !self.skips || max.is_some_and(|m| m <= 1);
        // the required copies: one failing fails the whole sequence, which restores the stack
        let (mid, fails) = self.steps(inner, s, u64::from(min), true, &mut empty);
        let err = fails.then_some(s);
        let Some(mid) = mid else {
            return Outcome {
                ok: None,
                err,
                empty: true,
            };
        };
        let ok = match max {
            Some(m) => {
                let optional = u64::from(m.saturating_sub(min));
                self.steps(inner, mid, optional, false, &mut empty).0
            }
            None => self.greedy(inner, mid, &mut empty),
        };
        Outcome { ok, err, empty }
    }

    /// `count` copies of `inner` from `s`, each starting where the previous one ended:
    /// `required` ones in sequence (each must succeed), or optional ones (`inner?`, which go
    /// on from the stack a failure leaves). Returns the stack after the last copy (`None` if a
    /// required copy cannot succeed) and whether a required copy can fail.
    ///
    /// The stack a copy starts with is a function of the stack the previous one started with,
    /// over a finite set of stacks, so the stacks repeat with some period: they are followed
    /// until one repeats, never past `count`, and the stack after `count` copies is read off
    /// the period. So huge bounds cost no more than small ones.
    fn steps(
        &mut self,
        inner: &'a ParserNode<'i>,
        s: AbsStack,
        count: u64,
        required: bool,
        empty: &mut bool,
    ) -> (Maybe, bool) {
        let mut trace: Vec<AbsStack> = vec![];
        let mut seen: HashMap<AbsStack, usize> = HashMap::new();
        let mut at = s;
        let mut fails = false;
        let mut t: u64 = 0;
        while t < count {
            if let Some(&j) = seen.get(&at) {
                // the stack before copy t is the one before copy j: from here they repeat
                // with period t - j, and every stack of the period has been visited already
                let period = t - j as u64;
                let end = trace[j + ((count - t) % period) as usize];
                return (Some(end), fails);
            }
            let _ = seen.insert(at, trace.len());
            trace.push(at);
            let o = self.visit(inner, at);
            if o.ok.is_some() {
                *empty &= o.empty;
            }
            t += 1;
            let next = if required {
                fails |= o.err.is_some();
                o.ok
            } else {
                join(o.ok, self.failed(o.err, at))
            };
            match next {
                Some(n) => at = n,
                None => return (None, fails),
            }
        }
        (Some(at), fails)
    }

    /// `inner*` from `s`: copies run until one fails, which ends the repetition. Returns every
    /// stack it can end with (`None` if no copy can fail: it never ends).
    fn greedy(&mut self, inner: &'a ParserNode<'i>, s: AbsStack, empty: &mut bool) -> Maybe {
        let mut seen = HashSet::new();
        let mut ok = None;
        let mut at = s;
        let mut first = true;
        while seen.insert(at) {
            let o = self.visit(inner, at);
            if o.ok.is_some() {
                *empty &= o.empty;
            }
            if o.err.is_some() {
                // The first copy runs in an optional, the later ones in a sequence that
                // restores the stack (in non-atomic rules), unless wrapped in RestoreOnErr.
                let restored = if first { None } else { Some(at) };
                ok = join(ok, join(self.failed(o.err, at), restored));
            }
            first = false;
            match o.ok {
                Some(n) => at = n,
                None => break,
            }
        }
        ok
    }

    /// A repetition of `body` entered with stack `s` never ends: from every stack `s` allows,
    /// `body` succeeds without consuming input, and the iterations come back to a stack an
    /// earlier iteration started with (after any number of steps), so they cycle forever.
    ///
    /// Pushed entries that can be non-blank are ones present before the cycle (an iteration
    /// that consumes nothing only pushes empty strings), so the same entries with the base
    /// untouched since the first of those iterations is the same concrete stack.
    fn loops_forever(&mut self, body: &'a ParserNode<'i>, s: AbsStack) -> bool {
        // each step lands in a finite set of states; this is more than enough to see a repeat
        const MAX_STEPS: usize = 64;
        let fresh = |st: AbsStack| AbsStack {
            base_intact: true,
            ..st
        };
        let mut cur = fresh(s);
        let mut seen = vec![cur];
        for _ in 0..MAX_STEPS {
            let Some(out) = self.outcome(body, cur).exact() else {
                return false;
            };
            if out.base_intact {
                if seen.iter().any(|earlier| out.same_top(*earlier)) {
                    return true;
                }
            } else {
                // the base changed: look for a cycle from here on
                seen.clear();
            }
            cur = fresh(out);
            seen.push(cur);
        }
        false
    }

    fn report(&mut self, node: &'a ParserNode<'i>, inner: &'a ParserNode<'i>) {
        // the existing check already reports these
        if is_non_failing(&inner.expr, self.rules, &mut vec![])
            || is_non_progressing(&inner.expr, self.rules, &mut vec![])
        {
            return;
        }
        if self.reported.insert(node as *const ParserNode<'i>) {
            self.errors.push(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "expression inside repetition is non-progressing and will repeat \
                              infinitely"
                        .to_owned(),
                },
                node.span,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use pest::Span;

    use crate::ast::RuleType;
    use crate::parser::{consume_rules, ParserExpr, ParserNode, ParserRule, PestParser, Rule};
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
            // an empty slice is always in range and matches nothing
            "a = { PEEK[0..0]* }",
            // the stack alternates between empty and one empty string: a two-step cycle
            "a = { POP_ALL ~ (DROP | (PUSH(\"\") ~ PEEK))* }",
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
            "WHITESPACE = _{ \" \" } a = { POP_ALL ~ ((PEEK_ALL ~ PEEK_ALL) | PEEK_ALL*) }",
            // the repetition is never reached: DROP fails on the empty stack
            "a = { POP_ALL ~ (DROP ~ PEEK_ALL*)? }",
            "a = { POP_ALL ~ (DROP ~ PUSH(\"\") ~ PEEK*)* }",
            "a = { POP_ALL ~ PUSH(\"a\") ~ DROP ~ DROP ~ POP_ALL ~ PEEK_ALL* }",
            // the only iteration allowed fails at DROP and takes the second alternative
            "a = { POP_ALL ~ ((DROP ~ PUSH(\"\") ~ PEEK*) | PUSH(\"\")){,1} }",
            // the ninth DROP always fails, so PEEK_ALL* is never reached
            "a = { (POP_ALL ~ PUSH(\"\"){8} ~ DROP{9} ~ PEEK_ALL*)? }",
            // eight iterations take the first alternative, the ninth pushes a blank: there is no
            // tenth iteration to run PEEK_ALL* on it
            "a = { POP_ALL ~ PUSH(\"x\"){8} ~ ((&DROP ~ PEEK_ALL* ~ DROP) | PUSH(\"\")){9} }",
            // the same with `{10,9}`: the optimizer runs nine copies, not ten
            "a = { POP_ALL ~ PUSH(\"x\"){8} ~ ((&DROP ~ PEEK_ALL* ~ DROP) | PUSH(\"\")){10,9} }",
            // a failed POP_ALL keeps the entries it removed: after PUSH(\"y\") ~ PUSH(\"x\"),
            // on \"yxy!\" it removes the blank and \"x\", then PEEK* matches \"y\" once
            "a = { PUSH(\"\") ~ (POP_ALL | PEEK*) } b = { PUSH(\"y\") ~ PUSH(\"x\") ~ a }",
            // a failed POP in a choice or an optional is restored: \"x\" stays, PEEK_ALL* stops
            "a = { POP_ALL ~ PUSH(\"x\") ~ (POP | PEEK_ALL*) }",
            "a = { POP_ALL ~ PUSH(\"x\") ~ POP? ~ PEEK_ALL* }",
            // the optimizer rotates choices, so only the bare `POP_ALL` branch is unwrapped: its
            // failure leaves the stack empty and the first DROP stops the last branch
            "a = { POP_ALL ~ PUSH(\"x\") ~ PUSH(\"\") ~ (!PUSH(\"\") | POP_ALL | (DROP ~ DROP ~ PEEK_ALL*)) }",
            // implicit whitespace between the iterations of a bounded repetition is captured
            "WHITESPACE = _{ \" \" } a = { PUSH(\"\"{2}) ~ PEEK* }",
            "WHITESPACE = _{ \" \" } b = !{ PEEK_ALL{2} } a = @{ POP_ALL ~ PUSH(b) ~ PEEK_ALL* }",
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

    /// After `POP_ALL`, the alternatives push one or two empty strings, depending on the input:
    /// afterwards the stack holds only empty strings, but how many is not known (the join of
    /// two all-blank stacks of different heights).
    #[test]
    fn stack_of_unknown_blank_height() {
        let blanks = "POP_ALL ~ ((&\"x\" ~ PUSH(\"\")) | (PUSH(\"\") ~ PUSH(\"\")))";
        // every entry is an empty string, so PEEK_ALL matches nothing: still a loop
        let errors = stack_loop_errors(&format!("a = {{ {blanks} ~ PEEK_ALL* }}"));
        assert_eq!(errors.len(), 1, "{errors:?}");
        // DROP on that stack (it holds at least one entry) leaves an all-blank stack of
        // unknown height again, so this is still a loop
        let errors = stack_loop_errors(&format!("a = {{ {blanks} ~ DROP ~ PEEK_ALL* }}"));
        assert_eq!(errors.len(), 1, "{errors:?}");
        // the second DROP fails after the one-entry branch, so this repetition ends
        let errors = stack_loop_errors(&format!("a = {{ {blanks} ~ (DROP ~ DROP)* }}"));
        assert_eq!(errors, Vec::<String>::new());
    }

    #[test]
    fn stack_of_empty_strings_replaced_by_an_empty_stack() {
        let blanks = "POP_ALL ~ ((&\"x\" ~ PUSH(\"\")) | (PUSH(\"\") ~ PUSH(\"\")))";
        // the first iteration changes the base (empty strings, then nothing), so the stacks
        // before it are not compared with the ones after it; from there the iterations cycle
        for body in ["POP_ALL ~ PUSH(\"\")", "PUSH(\"\") ~ POP_ALL"] {
            let errors = stack_loop_errors(&format!("a = {{ {blanks} ~ ({body})* }}"));
            assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        }
        // a choice whose branches leave an empty stack and a stack of empty strings, in either
        // order: PEEK_ALL matches nothing on both, but two DROPs fail on the empty one
        for choice in ["(&\"x\" ~ POP_ALL) | \"y\"", "\"y\" | (&\"x\" ~ POP_ALL)"] {
            let errors = stack_loop_errors(&format!("a = {{ {blanks} ~ ({choice}) ~ PEEK_ALL* }}"));
            assert_eq!(errors.len(), 1, "{choice}: {errors:?}");
            let errors =
                stack_loop_errors(&format!("a = {{ {blanks} ~ ({choice}) ~ (DROP ~ DROP)* }}"));
            assert_eq!(errors, Vec::<String>::new(), "{choice}");
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

    /// Like `validate_repetition`, this pass looks at a repetition's body, not at whether the
    /// input can reach it: the second branch below never runs, since the same "x" just failed,
    /// yet `validate_repetition` already rejects the `""*` form. The stack form is rejected the
    /// same way.
    #[test]
    fn unreachable_repetitions_are_reported_like_validate_repetition() {
        assert_eq!(
            stack_loop_errors("a = { \"x\" | (\"x\" ~ \"\"*) }").len(),
            1
        );
        assert_eq!(
            stack_loop_errors("a = { POP_ALL ~ (\"x\" | (\"x\" ~ PEEK_ALL*)) }").len(),
            1
        );
    }

    #[test]
    fn large_repetition_bounds_are_cheap() {
        // bounds are u32: iterations are not followed one by one up to the bound
        let start = std::time::Instant::now();
        assert_eq!(stack_loop_errors("a = { \"\"{1000000000,} }").len(), 1);
        assert_eq!(
            stack_loop_errors("a = { (PUSH(\"\") ~ POP){4294967295,} }").len(),
            1
        );
        assert_eq!(
            stack_loop_errors("a = { POP_ALL ~ (PUSH(\"\") ~ DROP){4294967295} ~ \"x\" }").len(),
            0
        );
        assert!(start.elapsed().as_secs() < 2, "{:?}", start.elapsed());
    }

    #[test]
    fn nested_bounded_repetitions_are_linear() {
        // every level revisits the same few stacks: each node is walked once per stack
        let mut body = "(DROP | PUSH(\"\"))".to_owned();
        for _ in 0..24 {
            body = format!("({body}){{3}}");
        }
        let input = format!("a = {{ POP_ALL ~ {body} }}");
        let start = std::time::Instant::now();
        assert_eq!(stack_loop_errors(&input), Vec::<String>::new());
        assert!(start.elapsed().as_secs() < 2, "{:?}", start.elapsed());
    }

    #[test]
    fn long_sequences_are_linear() {
        // Every prefix of a left-nested sequence is analysed once, not once per longer prefix
        // (rescanning prefixes took about 691,000 steps for 128 operands). Built directly, as
        // the parser's call limit stops a grammar this deep.
        let span = Span::new("a", 0, 1).unwrap();
        let node = |expr| ParserNode { expr, span };
        let mut seq = node(ParserExpr::Ident("POP_ALL".to_owned()));
        for _ in 0..4096 {
            seq = node(ParserExpr::Seq(
                Box::new(seq),
                Box::new(node(ParserExpr::Str("a".to_owned()))),
            ));
        }
        let peek_all = node(ParserExpr::Ident("PEEK_ALL".to_owned()));
        let body = node(ParserExpr::Seq(
            Box::new(seq),
            Box::new(node(ParserExpr::Rep(Box::new(peek_all)))),
        ));
        let rules = vec![ParserRule {
            name: "a".to_owned(),
            span,
            ty: RuleType::Normal,
            node: body,
        }];
        // the tree nests 4096 deep, so analyse it on a thread with a large stack
        let elapsed = std::thread::Builder::new()
            .stack_size(512 * 1024 * 1024)
            .spawn(move || {
                let start = std::time::Instant::now();
                assert_eq!(super::validate_stack_repetition(&rules).len(), 1);
                start.elapsed()
            })
            .unwrap()
            .join()
            .unwrap();
        assert!(elapsed.as_secs() < 2, "{elapsed:?}");
    }
}
