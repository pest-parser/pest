// pest. Smart PEGs in Rust
// Copyright (C) 2016  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A `struct` which caches `VecDeque`s.
#[derive(Debug)]
pub struct Queues<E> {
    queues: Vec<Vec<E>>,
    size: usize
}

impl<E> Queues<E> {
    /// Creates a new `Queues` `struct`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues = Queues::new();
    ///
    /// queues.last_mut().unwrap().push(1);
    /// queues.push();
    /// queues.last_mut().unwrap().push(2);
    ///
    /// queues.pour();
    ///
    /// assert_eq!(queues.last().unwrap()[0], 1);
    /// assert_eq!(queues.last().unwrap()[1], 2);
    /// ```
    pub fn new() -> Queues<E> {
        Queues {
            queues: vec![vec![]],
            size: 1
        }
    }

    /// Clears the `Queues` without resizing the cache.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues = Queues::new();
    ///
    /// queues.last_mut().unwrap().push(1);
    ///
    /// queues.clear();
    ///
    /// assert!(queues.last().unwrap().is_empty());
    /// ```
    #[inline]
    pub fn clear(&mut self) {
        for queue in &mut self.queues {
            queue.clear();
        }

        self.size = 1;
    }

    /// Returns the last `VecDeque` if size is greater than 1, else `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues: Queues<i32> = Queues::new();
    ///
    /// assert!(queues.last().unwrap().is_empty());
    /// ```
    #[inline]
    pub fn last(&self) -> Option<&Vec<E>> {
        if self.size > 0 {
            Some(&self.queues[self.size - 1])
        } else {
            None
        }
    }

    /// Returns the last `VecDeque` as mutable if size is greater than 1, else `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues: Queues<i32> = Queues::new();
    ///
    /// assert!(queues.last_mut().unwrap().is_empty());
    /// ```
    #[inline]
    pub fn last_mut(&mut self) -> Option<&mut Vec<E>> {
        if self.size > 0 {
            Some(&mut self.queues[self.size - 1])
        } else {
            None
        }
    }

    /// Pushes a new `VecDeque` at the end of a `Queues` `struct`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues = Queues::new();
    ///
    /// queues.last_mut().unwrap().push(1);
    /// queues.push();
    ///
    /// assert!(queues.last().unwrap().is_empty());
    /// ```
    #[inline]
    pub fn push(&mut self) {
        if self.size >= self.queues.len() {
            self.queues.push(vec![]);
        }

        self.size += 1;
    }

    /// Pops the last `VecDeque` from a `Queues` `struct`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues = Queues::new();
    ///
    /// queues.push();
    /// queues.last_mut().unwrap().push(1);
    /// queues.pop();
    ///
    /// assert!(queues.last().unwrap().is_empty());
    /// ```
    #[inline]
    pub fn pop(&mut self) {
        if self.size > 0 {
            self.queues[self.size - 1].clear();

            self.size -= 1;
        }
    }

    /// Pours all elements from the last `VecDeque` into the one that comes before it, and pops it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Queues;
    /// let mut queues = Queues::new();
    ///
    /// queues.last_mut().unwrap().push(1);
    /// queues.push();
    /// queues.last_mut().unwrap().push(2);
    ///
    /// queues.pour();
    ///
    /// assert_eq!(queues.last().unwrap()[0], 1);
    /// assert_eq!(queues.last().unwrap()[1], 2);
    /// ```
    #[inline]
    pub fn pour(&mut self) {
        if self.size > 1 {
            let (left, right) = self.queues.split_at_mut(self.size - 1);

            left[self.size - 2].append(&mut right[0]);

            self.size -= 1;
        }
    }
}
