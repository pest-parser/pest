// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use core::fmt::Debug;

use crate::{error::Error, Position, RuleType, Span, Stack};
pub use alloc::rc::Rc;

use super::predefined_node::EOI;

/// A node derived from a rule.
pub trait SubRule<R: RuleType> {
    /// The rule that the node belongs to.
    const RULE: R;
}

/// Node of concrete syntax tree that never fails.
pub trait NeverFailedTypedNode<'i>
where
    Self: Sized + Debug,
{
    /// Create typed node.
    /// `ATOMIC` refers to the external status, and it can be overriden by rule definition.
    fn parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> (Position<'i>, Self);
}

/// Node of concrete syntax tree.
pub trait TypedNode<'i, R: RuleType>
where
    Self: Sized + Debug,
{
    /// Create typed node.
    /// `ATOMIC` refers to the external status, and it can be overriden by rule definition.
    fn try_parse_with<const ATOMIC: bool>(
        input: Position<'i>,
        stack: &mut Stack<Span<'i>>,
    ) -> Result<(Position<'i>, Self), Error<R>>;
}

/// Node of concrete syntax tree.
pub trait ParsableTypedNode<'i, R: RuleType>
where
    Self: Sized + Debug + SubRule<R>,
{
    /// Parse the whole input into given typed node.
    /// A rule is not atomic by default.
    fn parse(input: &'i str) -> Result<Self, Error<R>>;
}

impl<'i, R: RuleType, T: TypedNode<'i, R> + SubRule<R>> ParsableTypedNode<'i, R> for T {
    #[inline]
    fn parse(input: &'i str) -> Result<Self, Error<R>> {
        let mut stack = Stack::new();
        let (input, res) = T::try_parse_with::<false>(Position::from_start(input), &mut stack)?;
        let (_, _) = EOI::try_parse_with::<false>(input, &mut stack)?;
        Ok(res)
    }
}
