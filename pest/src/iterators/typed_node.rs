// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use super::line_index::LineIndex;
use crate::{error::Error, RuleType};
pub use alloc::rc::Rc;

/// Wrapper for std::option::Option
#[cfg(feature = "std")]
pub type Option<T> = ::std::option::Option<T>;
/// Wrapper for std::option::Option
#[cfg(not(feature = "std"))]
pub type Option<T> = ::core::option::Option<T>;

/// Node of concrete syntax tree.
pub trait TypedNode<'i, R: RuleType>
where
    Self: Sized,
{
    /// Create typed node from tokens
    fn try_from(
        input: &'i str,
        line_index: Option<Rc<LineIndex>>,
    ) -> Result<(&'i str, Self), Error<R>>;
}
