// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::rc::Rc;
use alloc::vec::Vec;

use super::line_index::LineIndex;
use super::pairs::{self, Pairs};
use super::queueable_token::QueueableToken;
use crate::RuleType;

/// A builder that constructs [`Pairs`] manually, without running a parser.
///
/// This is meant for unit tests: code that consumes parser output often takes a
/// [`Pair`] or [`Pairs`] as input, and testing it would otherwise require
/// running a real parse just to obtain the right shape of tokens. With a
/// `PairsBuilder` the expected tree can be spelled out directly.
///
/// A pair is described by a [`Rule`], a `start` byte offset and an `end` byte
/// offset into `input` (the same `[start, end)` convention used by
/// [`Span`]), and, optionally, inner pairs and a node tag.
///
/// [`Pair`]: struct.Pair.html
/// [`Rule`]: ../trait.RuleType.html
/// [`Span`]: ../struct.Span.html
///
/// # Examples
///
/// Building a single leaf pair — the motivating use case, a `Pair` handed to a
/// function under test without invoking the parser:
///
/// ```
/// # use pest::iterators::PairsBuilder;
/// # #[allow(non_camel_case_types)]
/// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// enum Rule {
///     integer,
/// }
///
/// let pairs = PairsBuilder::new("42").rule(Rule::integer, 0, 2).build();
/// let pair = pairs.peek().unwrap();
///
/// assert_eq!(pair.as_rule(), Rule::integer);
/// assert_eq!(pair.as_str(), "42");
/// ```
///
/// Nesting inner pairs with [`rule_with`] and attaching tags with [`tag`]:
///
/// ```
/// # use pest::iterators::PairsBuilder;
/// # #[allow(non_camel_case_types)]
/// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// enum Rule {
///     sum,
///     number,
/// }
///
/// // Describes the parse tree of "1+2": a `sum` containing two `number`s.
/// let pairs = PairsBuilder::new("1+2")
///     .rule_with(Rule::sum, 0, 3, |inner| {
///         inner
///             .rule(Rule::number, 0, 1)
///             .tag("lhs")
///             .rule(Rule::number, 2, 3)
///             .tag("rhs")
///     })
///     .build();
///
/// let sum = pairs.peek().unwrap();
/// assert_eq!(sum.as_rule(), Rule::sum);
///
/// let numbers: Vec<_> = sum.into_inner().map(|p| p.as_str()).collect();
/// assert_eq!(numbers, ["1", "2"]);
/// ```
///
/// [`rule_with`]: struct.PairsBuilder.html#method.rule_with
/// [`tag`]: struct.PairsBuilder.html#method.tag
#[derive(Clone, Debug)]
pub struct PairsBuilder<'i, R> {
    input: &'i str,
    nodes: Vec<BuilderNode<'i, R>>,
}

#[derive(Clone, Debug)]
struct BuilderNode<'i, R> {
    rule: R,
    start: usize,
    end: usize,
    tag: Option<&'i str>,
    children: Vec<BuilderNode<'i, R>>,
}

impl<'i, R: RuleType> PairsBuilder<'i, R> {
    /// Creates an empty builder over `input`.
    ///
    /// All offsets passed to [`rule`] and [`rule_with`] index into this string.
    ///
    /// [`rule`]: struct.PairsBuilder.html#method.rule
    /// [`rule_with`]: struct.PairsBuilder.html#method.rule_with
    pub fn new(input: &'i str) -> Self {
        PairsBuilder {
            input,
            nodes: Vec::new(),
        }
    }

    /// Appends a leaf [`Pair`] for `rule` spanning `input[start..end]`.
    ///
    /// [`Pair`]: struct.Pair.html
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::iterators::PairsBuilder;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule { a, b }
    /// let pairs = PairsBuilder::new("a b")
    ///     .rule(Rule::a, 0, 1)
    ///     .rule(Rule::b, 2, 3)
    ///     .build();
    ///
    /// let strings: Vec<_> = pairs.map(|p| p.as_str()).collect();
    /// assert_eq!(strings, ["a", "b"]);
    /// ```
    pub fn rule(mut self, rule: R, start: usize, end: usize) -> Self {
        self.nodes.push(BuilderNode {
            rule,
            start,
            end,
            tag: None,
            children: Vec::new(),
        });
        self
    }

    /// Appends a [`Pair`] for `rule` spanning `input[start..end]`, whose inner
    /// [`Pairs`] are produced by `build_children`.
    ///
    /// `build_children` receives a fresh builder over the same `input`; the
    /// pairs it appends become the inner pairs of this rule, in the order they
    /// are added.
    ///
    /// [`Pair`]: struct.Pair.html
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::iterators::PairsBuilder;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule { list, item }
    /// let pairs = PairsBuilder::new("[a,b]")
    ///     .rule_with(Rule::list, 0, 5, |inner| {
    ///         inner.rule(Rule::item, 1, 2).rule(Rule::item, 3, 4)
    ///     })
    ///     .build();
    ///
    /// let list = pairs.peek().unwrap();
    /// assert_eq!(list.into_inner().count(), 2);
    /// ```
    pub fn rule_with<F>(mut self, rule: R, start: usize, end: usize, build_children: F) -> Self
    where
        F: FnOnce(PairsBuilder<'i, R>) -> PairsBuilder<'i, R>,
    {
        let children = build_children(PairsBuilder::new(self.input)).nodes;
        self.nodes.push(BuilderNode {
            rule,
            start,
            end,
            tag: None,
            children,
        });
        self
    }

    /// Attaches `tag` to the most recently appended pair, so that it can be
    /// looked up with [`Pairs::find_first_tagged`], [`Pairs::find_tagged`] or
    /// [`Pair::as_node_tag`].
    ///
    /// [`Pairs::find_first_tagged`]: struct.Pairs.html#method.find_first_tagged
    /// [`Pairs::find_tagged`]: struct.Pairs.html#method.find_tagged
    /// [`Pair::as_node_tag`]: struct.Pair.html#method.as_node_tag
    ///
    /// # Panics
    ///
    /// Panics if called before any rule has been appended to this builder.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::iterators::PairsBuilder;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule { number }
    /// let pairs = PairsBuilder::new("7").rule(Rule::number, 0, 1).tag("answer").build();
    ///
    /// assert_eq!(pairs.find_first_tagged("answer").unwrap().as_str(), "7");
    /// ```
    pub fn tag(mut self, tag: &'i str) -> Self {
        self.nodes
            .last_mut()
            .expect("PairsBuilder::tag called before any rule was added")
            .tag = Some(tag);
        self
    }

    /// Consumes the builder and produces the [`Pairs`].
    ///
    /// # Panics
    ///
    /// Panics if any appended rule's `[start, end)` is not a valid range into
    /// `input` — that is, if `start > end`, `end` is past the end of `input`, or
    /// either offset does not fall on a UTF-8 character boundary. This mirrors
    /// the invariant enforced by [`Span::new`], which the resulting pairs would
    /// otherwise violate.
    ///
    /// [`Span::new`]: ../struct.Span.html#method.new
    pub fn build(self) -> Pairs<'i, R> {
        let mut queue = Vec::new();

        for node in &self.nodes {
            push_node(&mut queue, self.input, node);
        }

        let end = queue.len();

        pairs::new(
            Rc::new(queue),
            self.input,
            Some(Rc::new(LineIndex::new(self.input))),
            0,
            end,
        )
    }
}

/// Flattens `node` (and its descendants, depth-first) into the token `queue`,
/// preserving the `Start`/`End` matched-index invariant that [`Pairs`] relies
/// on for O(1) navigation.
fn push_node<'i, R: RuleType>(
    queue: &mut Vec<QueueableToken<'i, R>>,
    input: &'i str,
    node: &BuilderNode<'i, R>,
) {
    assert!(
        input.get(node.start..node.end).is_some(),
        "PairsBuilder: invalid span {}..{} for input of length {}; \
         start..end must be an ascending range on UTF-8 character boundaries",
        node.start,
        node.end,
        input.len()
    );

    let start_index = queue.len();
    queue.push(QueueableToken::Start {
        end_token_index: 0,
        input_pos: node.start,
    });

    for child in &node.children {
        push_node(queue, input, child);
    }

    let end_index = queue.len();
    match queue[start_index] {
        QueueableToken::Start {
            ref mut end_token_index,
            ..
        } => *end_token_index = end_index,
        // We only ever push a `Start` at `start_index` just above.
        _ => unreachable!(),
    }

    queue.push(QueueableToken::End {
        start_token_index: start_index,
        rule: node.rule,
        tag: node.tag,
        input_pos: node.end,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    enum Rule {
        a,
        b,
        sum,
        number,
    }

    #[test]
    fn single_leaf() {
        let pairs = PairsBuilder::new("42").rule(Rule::number, 0, 2).build();
        let pair = pairs.peek().unwrap();

        assert_eq!(pair.as_rule(), Rule::number);
        assert_eq!(pair.as_str(), "42");
        assert_eq!(pair.as_span().start(), 0);
        assert_eq!(pair.as_span().end(), 2);
        assert_eq!(pair.into_inner().count(), 0);
    }

    #[test]
    fn multiple_top_level_leaves() {
        let mut pairs = PairsBuilder::new("a b")
            .rule(Rule::a, 0, 1)
            .rule(Rule::b, 2, 3)
            .build();

        assert_eq!(pairs.len(), 2);

        let first = pairs.next().unwrap();
        assert_eq!(first.as_rule(), Rule::a);
        assert_eq!(first.as_str(), "a");

        let second = pairs.next().unwrap();
        assert_eq!(second.as_rule(), Rule::b);
        assert_eq!(second.as_str(), "b");

        assert!(pairs.next().is_none());
    }

    #[test]
    fn nested_pairs() {
        let pairs = PairsBuilder::new("1+2")
            .rule_with(Rule::sum, 0, 3, |inner| {
                inner.rule(Rule::number, 0, 1).rule(Rule::number, 2, 3)
            })
            .build();

        let sum = pairs.peek().unwrap();
        assert_eq!(sum.as_rule(), Rule::sum);
        assert_eq!(sum.as_str(), "1+2");

        let inner: Vec<_> = sum.into_inner().collect();
        assert_eq!(inner.len(), 2);
        assert_eq!(inner[0].as_str(), "1");
        assert_eq!(inner[0].as_rule(), Rule::number);
        assert_eq!(inner[1].as_str(), "2");
        assert_eq!(inner[1].as_rule(), Rule::number);
    }

    #[test]
    fn tokens_round_trip() {
        use crate::Token;

        let pairs = PairsBuilder::new("1+2")
            .rule_with(Rule::sum, 0, 3, |inner| {
                inner.rule(Rule::number, 0, 1).rule(Rule::number, 2, 3)
            })
            .build();

        let tokens: Vec<_> = pairs.tokens().collect();
        assert_eq!(
            tokens,
            vec![
                Token::Start {
                    rule: Rule::sum,
                    pos: pos("1+2", 0)
                },
                Token::Start {
                    rule: Rule::number,
                    pos: pos("1+2", 0)
                },
                Token::End {
                    rule: Rule::number,
                    pos: pos("1+2", 1)
                },
                Token::Start {
                    rule: Rule::number,
                    pos: pos("1+2", 2)
                },
                Token::End {
                    rule: Rule::number,
                    pos: pos("1+2", 3)
                },
                Token::End {
                    rule: Rule::sum,
                    pos: pos("1+2", 3)
                },
            ]
        );
    }

    fn pos(input: &str, pos: usize) -> crate::Position<'_> {
        crate::Position::new(input, pos).unwrap()
    }

    #[test]
    fn tags_are_queryable() {
        let pairs = PairsBuilder::new("1+2")
            .rule_with(Rule::sum, 0, 3, |inner| {
                inner
                    .rule(Rule::number, 0, 1)
                    .tag("lhs")
                    .rule(Rule::number, 2, 3)
                    .tag("rhs")
            })
            .build();

        assert_eq!(
            pairs.clone().find_first_tagged("lhs").unwrap().as_str(),
            "1"
        );
        assert_eq!(
            pairs.clone().find_first_tagged("rhs").unwrap().as_str(),
            "2"
        );
        assert!(pairs.find_first_tagged("missing").is_none());
    }

    #[test]
    fn empty_builder_yields_no_pairs() {
        let pairs = PairsBuilder::<Rule>::new("").build();
        assert!(pairs.clone().peek().is_none());
        assert_eq!(pairs.count(), 0);
    }

    #[test]
    fn line_col_is_computed() {
        let pairs = PairsBuilder::new("ab\ncd").rule(Rule::a, 3, 5).build();
        let pair = pairs.peek().unwrap();
        assert_eq!(pair.line_col(), (2, 1));
    }

    #[test]
    fn multibyte_span() {
        // "héllo": 'é' is two bytes, so byte offset 3 lands right after it.
        let pairs = PairsBuilder::new("héllo").rule(Rule::a, 0, 3).build();
        assert_eq!(pairs.peek().unwrap().as_str(), "hé");
    }

    #[test]
    #[should_panic(expected = "invalid span")]
    fn out_of_bounds_span_panics() {
        let _ = PairsBuilder::new("ab").rule(Rule::a, 0, 5).build();
    }

    #[test]
    #[should_panic(expected = "invalid span")]
    fn non_char_boundary_span_panics() {
        // Byte offset 1 splits the two-byte 'é'.
        let _ = PairsBuilder::new("é").rule(Rule::a, 0, 1).build();
    }

    #[test]
    #[should_panic(expected = "before any rule")]
    fn tag_without_rule_panics() {
        let _ = PairsBuilder::<Rule>::new("x").tag("oops");
    }
}
