// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::Arc;

use super::input::Input;
use super::position;

#[derive(Eq)]
pub struct Span<I: Input> {
    input: Rc<Arc<I>>,
    start: usize,
    end:   usize
}

#[inline]
pub fn new<I: Input>(input: Rc<Arc<I>>, start: usize, end: usize) -> Span<I> {
    Span {
        input: input,
        start: start,
        end:   end
    }
}

impl<I: Input> Span<I> {
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    #[inline]
    pub fn split(self) -> (position::Position<I>, position::Position<I>) {
        let pos1 = position::new(self.input.clone(), self.start);
        let pos2 = position::new(self.input, self.end);

        (pos1, pos2)
    }

    #[inline]
    pub fn capture(&self) -> &str {
        unsafe { self.input.slice(self.start, self.end) }
    }
}

impl<I: Input> fmt::Debug for Span<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span {{ start: {}, end: {} }}", self.start, self.end)
    }
}

impl<I: Input> Clone for Span<I> {
    fn clone(&self) -> Span<I> {
        new(self.input.clone(), self.start, self.end)
    }
}

impl<I: Input> PartialEq for Span<I> {
    fn eq(&self, other: &Span<I>) -> bool {
        &**self.input as *const I == &**other.input as *const I &&
        self.start == other.start &&
        self.end == other.end
    }
}

impl<'a, I: Input> Hash for Span<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&**self.input as *const I).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
