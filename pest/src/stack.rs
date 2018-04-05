// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

/// Implementation of a `Stack` which maintains an log of `StackOp`s in order to rewind the stack
/// to a previous state.

use std::fmt::Debug;

#[derive(Debug)]
pub struct Stack<T: Clone + Debug> {
    ops: Vec<StackOp<T>>,
    cache: Vec<T>,
    snapshots: Vec<usize>,
    current_snapshot: usize
}

impl<T: Clone + Debug> Stack<T> {
    /// Creates a new `Stack`.
    pub fn new() -> Self {
        Stack {
            ops: vec![],
            cache: vec![],
            snapshots: vec![],
            current_snapshot: 0
        }
    }

    /// Returns `true` if the stack is currently empty.
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
        if let Some(val) = popped.clone() {
            self.ops.push(StackOp::Pop(val));
        }
        popped
    }

    /// Takes a snapshot of the current `Stack`.
    pub fn snapshot(&mut self) {
        let ops_index = self.ops.len();
        if ops_index > self.current_snapshot {
            self.snapshots.push(ops_index);
            self.current_snapshot = ops_index;
        }
    }

    /// Rewinds the `Stack` to the most recent `snapshot()`. If no `snapshot()` has been taken, this
    /// function will do nothing.
    pub fn backtrack(&mut self) {
        if let Some(ops_index) = self.snapshots.pop() {
            self.rewind_to(ops_index);
            self.ops.truncate(ops_index);
            self.current_snapshot = self.ops.len();
        }
    }

    // Rewind the stack to a particular index
    fn rewind_to(&mut self, index: usize) {
        let ops_to_rewind = &self.ops[index..];
        for op in ops_to_rewind.iter().rev() {
            match op {
                &StackOp::Push(_) => {
                    self.cache.pop();
                },
                &StackOp::Pop(ref elem) => {
                    self.cache.push(elem.clone());
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackOp<T> {
    Push(T),
    Pop(T)
}

#[cfg(test)]
mod test {
    use super::Stack;

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
        stack.backtrack();
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(0));
        assert_eq!(stack.pop(), None);

        // Test backtracking
        // [0, 2, 3]
        stack.backtrack();
        assert_eq!(stack.pop(), Some(3));
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(0));
        assert_eq!(stack.pop(), None);
    }
}
