// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

/// Implementation of a `Stack` which maintains a log of `StackOp`s in order to rewind the stack
/// to a previous state.
#[derive(Debug)]
pub struct Stack<T: Clone> {
    ops: Vec<StackOp<T>>,
    cache: Vec<T>,
    snapshots: Vec<usize>
}

impl<T: Clone> Stack<T> {
    /// Creates a new `Stack`.
    pub fn new() -> Self {
        Stack {
            ops: vec![],
            cache: vec![],
            snapshots: vec![]
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
        self.ops.push(StackOp::Pop);
        self.cache.pop()
    }

    /// Clears all the values from the `Stack`.
    pub fn clear(&mut self) {
        self.ops.push(StackOp::Clear);
        self.cache.clear();
    }

    /// Takes a snapshot of the current `Stack`.
    pub fn snapshot(&mut self) {
        self.snapshots.push(self.ops.len());
    }

    /// Rewinds the `Stack` to the most `snapshot()`. If no `snapshot()` has been taken, this
    /// function will do nothing.
    pub fn backtrack(&mut self) {
        match self.snapshots.pop() {
            Some(index) => {
                self.ops.truncate(index);
                self.recache();
            }
            None => {}
        }
    }

    // Rebuilds the internal cache based on the previous operations stored.
    fn recache(&mut self) {
        self.cache.clear();
        for op in self.ops.iter() {
            match op {
                &StackOp::Push(ref elem) => {
                    self.cache.push(elem.clone());
                },
                &StackOp::Pop => {
                    self.cache.pop();
                },
                &StackOp::Clear => {
                    self.cache.clear();
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackOp<T> {
    Push(T),
    Pop,
    Clear
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
        assert_eq!(stack.peek(), None);
        assert_eq!(stack.pop(), None);

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
