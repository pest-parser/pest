// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

use super::input::Input;
use super::span;

#[derive(Debug, Eq, Ord)]
pub struct Position<I: Input> {
    input: Rc<Arc<I>>,
    pos:   usize
}

#[inline]
pub fn new<I: Input>(input: Rc<Arc<I>>, pos: usize) -> Position<I> {
    Position {
        input: input,
        pos:   pos
    }
}

impl<I: Input> Position<I> {
    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    pub fn at_start(&self) -> bool {
        self.pos == 0
    }

    #[inline]
    pub fn at_end(&self) -> bool {
        self.pos == self.input.len()
    }

    #[inline]
    pub fn line_col(&self) -> (usize, usize) {
        unsafe { self.input.line_col(self.pos) }
    }

    #[inline]
    pub fn line_of(&self) -> &str {
        unsafe { self.input.line_of(self.pos) }
    }

    #[inline]
    pub fn span(self, other: Position<I>) -> span::Span<I> {
        if &**self.input as *const I == &**other.input as *const I {
            span::new(self.input, self.pos, other.pos)
        } else {
            panic!("Span created from positions coming from different inputs")
        }
    }

    #[inline]
    pub fn match_string(&self, string: &str) -> Option<Position<I>> {
        if unsafe { self.input.match_string(string, self.pos) } {
            Some(new(self.input.clone(), self.pos + string.len()))
        } else {
            None
        }
    }

    #[inline]
    pub fn match_insensitive(&self, string: &str) -> Option<Position<I>> {
        if unsafe { self.input.match_insensitive(string, self.pos) } {
            Some(new(self.input.clone(), self.pos + string.len()))
        } else {
            None
        }
    }

    #[inline]
    pub fn match_range(&self, range: Range<char>) -> Option<Position<I>> {
        let len = unsafe { self.input.match_range(range, self.pos) };
        len.map(|len| new(self.input.clone(), self.pos + len))
    }

    #[inline]
    pub fn repeat<F>(self, mut f: F) -> Option<Position<I>>
        where F: FnMut(Position<I>) -> Option<Position<I>> {

        let mut option = Some(self);

        loop {
            let result = if let Some(ref pos) = option {
                f(pos.clone())
            } else {
                unreachable!();
            };

            if result.is_some() {
                option = result;
            } else {
                break;
            }
        }

        option
    }
}

impl<I: Input> Clone for Position<I> {
    fn clone(&self) -> Position<I> {
        new(self.input.clone(), self.pos)
    }
}

impl<I: Input> PartialEq for Position<I> {
    fn eq(&self, other: &Position<I>) -> bool {
        &**self.input as *const I == &**other.input as *const I &&
        self.pos == other.pos
    }
}

impl<I: Input> PartialOrd for Position<I> {
    fn partial_cmp(&self, other: &Position<I>) -> Option<Ordering> {
        if &**self.input as *const I == &**other.input as *const I {
            self.pos.partial_cmp(&other.pos)
        } else {
            None
        }
    }
}

impl<I: Input> Hash for Position<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&**self.input as *const I).hash(state);
        self.pos.hash(state);
    }
}
