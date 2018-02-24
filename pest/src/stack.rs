// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[derive(Debug)]
pub struct Stack<T: Clone> {
    ops: Vec<StackOp<T>>,
    cache: Vec<T>,
    snapshots: Vec<usize>
}

impl<T: Clone> Stack<T> {
    pub fn new() -> Self {
        Stack {
            ops: vec![],
            cache: vec![],
            snapshots: vec![]
        }
    }

    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    pub fn peek(&self) -> Option<&T> {
        self.cache.last()
    }

    pub fn push(&mut self, elem: T) {
        self.ops.push(StackOp::Push(elem.clone()));
        self.cache.push(elem);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.ops.push(StackOp::Pop());
        self.cache.pop()
    }

    pub fn snapshot(&mut self) {
        self.snapshots.push(self.ops.len());
    }

    pub fn backtrack(&mut self) {
        match self.snapshots.pop() {
            Some(index) => {
                self.ops.truncate(index);
                self.recache();
            }
            None => {}
        }
    }

    fn recache(&mut self) {
        self.cache.clear();
        for op in self.ops.iter() {
            match op {
                &StackOp::Push(ref elem) => {
                    self.cache.push(elem.clone());
                },
                &StackOp::Pop() => {
                    self.cache.pop();
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackOp<T> {
    Push(T),
    Pop()
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
