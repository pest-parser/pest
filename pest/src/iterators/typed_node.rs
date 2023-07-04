// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

// use alloc::{rc::Rc, vec::Vec};

use crate::RuleType;

// use super::{line_index::LineIndex, QueueableToken};

/// Node of concrete syntax tree.
pub trait TypedNode<'i, R: RuleType> {
    /*
    /// Create typed node from tokens
    fn new(
        queue: Rc<Vec<QueueableToken<'i, R>>>,
        input: &'i str,
        line_index: Option<Rc<LineIndex>>,
        start: usize,
        end: usize,
    ) -> Self;
    */
}
