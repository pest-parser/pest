// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::vec;
use alloc::vec::Vec;
use core::ops::{Index, Range};

/// Implementation of a `Stack` which maintains an log of `StackOp`s in order to rewind the stack
/// to a previous state.
#[derive(Debug)]
pub struct Stack<T: Clone> {
    ops: Vec<StackOp<T>>,
    cache: Vec<T>,
    snapshots: Vec<usize>,
}

impl<T: Clone> Stack<T> {
    /// Creates a new `Stack`.
    pub fn new() -> Self {
        Stack {
            ops: vec![],
            cache: vec![],
            snapshots: vec![],
        }
    }

    /// Returns `true` if the stack is currently empty.
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Returns the top-most `&T` in the `Stack`.
    pub fn peek(&self) -> Option<&T> {
        self.cache.last()
    }

    /// Pushes a `T` onto the `Stack`.
    pub fn push(&mut self, elem: T) {
        self.ops.push(StackOp::Push(elem.clone()));
        self.cache.push(elem);
    }

    /// Pops the top-most `T` from the `Stack`.
    pub fn pop(&mut self) -> Option<T> {
        let popped = self.cache.pop();
        if let Some(ref val) = popped {
            self.ops.push(StackOp::Pop(val.clone()));
        }
        popped
    }

    /// Returns the size of the stack
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Takes a snapshot of the current `Stack`.
    pub fn snapshot(&mut self) {
        self.snapshots.push(self.ops.len());
    }

    /// The parsing after the last snapshot was successful so clearing it.
    pub fn clear_snapshot(&mut self) {
        self.snapshots.pop();
    }

    /// Rewinds the `Stack` to the most recent `snapshot()`. If no `snapshot()` has been taken, this
    /// function return the stack to its initial state.
    pub fn restore(&mut self) {
        match self.snapshots.pop() {
            Some(ops_index) => {
                self.rewind_to(ops_index);
                self.ops.truncate(ops_index);
            }
            None => {
                self.cache.clear();
                self.ops.clear();
            }
        }
    }

    // Rewind the stack to a particular index
    fn rewind_to(&mut self, index: usize) {
        let ops_to_rewind = &self.ops[index..];
        for op in ops_to_rewind.iter().rev() {
            match *op {
                StackOp::Push(_) => {
                    self.cache.pop();
                }
                StackOp::Pop(ref elem) => {
                    self.cache.push(elem.clone());
                }
            }
        }
    }
}

impl<T: Clone> Index<Range<usize>> for Stack<T> {
    type Output = [T];

    fn index(&self, range: Range<usize>) -> &[T] {
        self.cache.index(range)
    }
}

#[derive(Debug)]
enum StackOp<T> {
    Push(T),
    Pop(T),
}

#[cfg(test)]
mod test {
    use super::Stack;

    #[test]
    fn snapshot_with_empty() {
        let mut stack = Stack::new();

        stack.snapshot();
        // []
        assert!(stack.is_empty());
        // [0]
        stack.push(0);
        stack.restore();
        assert!(stack.is_empty());
    }

    #[test]
    fn snapshot_twice() {
        let mut stack = Stack::new();

        stack.push(0);

        stack.snapshot();
        stack.snapshot();
        stack.restore();
        stack.restore();

        assert_eq!(stack[0..stack.len()], [0]);
    }

    #[test]
    fn stack_ops() {
        let mut stack = Stack::new();

        // []
        assert!(stack.is_empty());
        assert_eq!(stack.peek(), None);
        assert_eq!(stack.pop(), None);

        // [0]
        stack.push(0);
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&0));

        // [0, 1]
        stack.push(1);
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&1));

        // [0]
        assert_eq!(stack.pop(), Some(1));
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&0));

        // [0, 2]
        stack.push(2);
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&2));

        // [0, 2, 3]
        stack.push(3);
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&3));

        // Take a snapshot of the current stack
        // [0, 2, 3]
        stack.snapshot();

        // [0, 2]
        assert_eq!(stack.pop(), Some(3));
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&2));

        // Take a snapshot of the current stack
        // [0, 2]
        stack.snapshot();

        // [0]
        assert_eq!(stack.pop(), Some(2));
        assert!(!stack.is_empty());
        assert_eq!(stack.peek(), Some(&0));

        // []
        assert_eq!(stack.pop(), Some(0));
        assert!(stack.is_empty());

        // Test backtracking
        // [0, 2]
        stack.restore();
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(0));
        assert_eq!(stack.pop(), None);

        // Test backtracking
        // [0, 2, 3]
        stack.restore();
        assert_eq!(stack.pop(), Some(3));
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(0));
        assert_eq!(stack.pop(), None);
    }
}
