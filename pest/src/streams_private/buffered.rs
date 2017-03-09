// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::UnsafeCell;
use std::ptr;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};
use std::vec::Drain;

use futures::{Async, Poll};
use futures::stream::Stream;
use futures::task::{park, Task};

struct RingBuffer<T, E> {
    buffer:        UnsafeCell<Vec<Option<T>>>,
    read:          AtomicUsize,
    write:         AtomicUsize,
    pub error:     AtomicPtr<E>,
    pub is_closed: AtomicBool,
    pub task:      AtomicPtr<Task>,
    capacity:      usize,
    size:          usize
}

impl<T, E> RingBuffer<T, E> {
    #[inline]
    pub fn new(capacity: usize) -> RingBuffer<T, E> {
        let size = capacity.next_power_of_two();

        RingBuffer {
            buffer:    UnsafeCell::new(Vec::with_capacity(size)),
            read:      AtomicUsize::new(0),
            write:     AtomicUsize::new(0),
            error:     AtomicPtr::new(ptr::null_mut()),
            is_closed: AtomicBool::new(false),
            task:      AtomicPtr::new(ptr::null_mut()),
            capacity:  capacity,
            size:      size
        }
    }

    #[inline]
    pub fn try_push(&self, value: T) -> Option<T> {
        let read = self.read.load(Ordering::Relaxed);
        let write = self.write.load(Ordering::Relaxed);

        if write - read < self.capacity {
            self.push(write, value);

            self.write.store(write.wrapping_add(1), Ordering::Relaxed);

            self.try_unpark();

            None
        } else {
            Some(value)
        }
    }

    #[inline]
    pub fn try_push_all<'a>(&self, mut drain: Drain<'a, T>) -> Option<Drain<'a, T>> {
        let read = self.read.load(Ordering::Relaxed);
        let write = self.write.load(Ordering::Relaxed);
        let mut difference = write - read;

        if difference < self.capacity {
            while let Some(value) = drain.next() {
                self.push(write, value);
                write.wrapping_add(1);

                difference -= 1;

                if difference == 0 {
                    break;
                }
            }

            self.try_unpark();

            if drain.len() != 0 {
                Some(drain)
            } else {
                None
            }
        } else {
            Some(drain)
        }
    }

    #[inline]
    pub fn try_pop(&self) -> Option<T> {
        let read = self.read.load(Ordering::Relaxed);
        let write = self.write.load(Ordering::Relaxed);

        if read != write {
            let mut buffer = unsafe { &mut *self.buffer.get() };
            let result = buffer[self.index(read)].take().unwrap();

            self.read.store(read.wrapping_add(1), Ordering::Relaxed);

            Some(result)
        } else {
            None
        }
    }

    #[inline]
    pub fn try_unpark(&self) {
        let task = self.task.load(Ordering::Acquire);

        if !task.is_null() {
            unsafe { Box::from_raw(task).unpark(); }
            self.task.store(ptr::null_mut(), Ordering::Release);
        }
    }

    #[inline]
    fn push(&self, index: usize, value: T) {
        let mut buffer = unsafe { &mut *self.buffer.get() };

        if buffer.len() < buffer.capacity() {
            buffer.push(Some(value));
        } else {
            buffer[self.index(index)] = Some(value);
        }
    }

    #[inline]
    fn index(&self, index: usize) -> usize {
        index & (self.size - 1)
    }
}

unsafe impl<T: Sync, E: Sync> Sync for RingBuffer<T, E> {}

pub struct BufferedStream<T, E> {
    buffer: Arc<RingBuffer<T, E>>
}

impl<T, E> Stream for BufferedStream<T, E> {
    type Item  = T;
    type Error = E;

    fn poll(&mut self) -> Poll<Option<T>, E> {
        let error = self.buffer.error.load(Ordering::Acquire);

        if !error.is_null() {
            let error = unsafe { Err(*Box::from_raw(error)) };
            self.buffer.error.store(ptr::null_mut(), Ordering::Release);

            return error;
        }

        match self.buffer.try_pop() {
            Some(value) => Ok(Async::Ready(Some(value))),
            None => {
                if self.buffer.is_closed.load(Ordering::Relaxed) {
                    Ok(Async::Ready(None))
                } else {
                    let boxed = Box::new(park());
                    self.buffer.task.store(Box::into_raw(boxed), Ordering::Release);

                    Ok(Async::NotReady)
                }
            }
        }
    }
}

pub struct BufferedSender<T, E> {
    buffer: Arc<RingBuffer<T, E>>
}

impl<T, E> BufferedSender<T, E> {
    #[inline]
    pub fn send(&self, value: T) {
        let mut value = value;

        loop {
            value = match self.buffer.try_push(value) {
                Some(value) => value,
                None        => break
            };
        }
    }

    #[inline]
    pub fn send_all(&self, drain: Drain<T>) {
        let mut drain = drain;

        loop {
            drain = match self.buffer.try_push_all(drain) {
                Some(drain) => drain,
                None        => break
            };
        }
    }

    #[inline]
    pub fn fail(self, error: E) {
        let boxed = Box::new(error);
        self.buffer.error.store(Box::into_raw(boxed), Ordering::Release);
    }
}

impl<T, E> Drop for BufferedSender<T, E> {
    fn drop(&mut self) {
        self.buffer.try_unpark();

        self.buffer.is_closed.store(true, Ordering::Relaxed);
    }
}

pub fn buffered<T, E>(capacity: usize) -> (BufferedSender<T, E>, BufferedStream<T, E>) {
    let buffer = Arc::new(RingBuffer::new(capacity));

    (BufferedSender { buffer: buffer.clone() }, BufferedStream { buffer: buffer })
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    use futures::future::Future;
    use futures::stream::Stream;

    use super::*;

    #[test]
    fn ring_push_pop() {
        let ring: RingBuffer<i32, ()> = RingBuffer::new(3);

        assert_eq!(ring.try_push(1), None);
        assert_eq!(ring.try_push(2), None);
        assert_eq!(ring.try_push(3), None);
        assert_eq!(ring.try_push(4), Some(4));

        assert_eq!(ring.try_pop(), Some(1));
        assert_eq!(ring.try_pop(), Some(2));
        assert_eq!(ring.try_pop(), Some(3));
        assert_eq!(ring.try_pop(), None);

        assert_eq!(ring.try_push(1), None);
        assert_eq!(ring.try_push(2), None);
        assert_eq!(ring.try_push(3), None);
        assert_eq!(ring.try_push(4), Some(4));

        assert_eq!(ring.try_pop(), Some(1));
        assert_eq!(ring.try_pop(), Some(2));
        assert_eq!(ring.try_pop(), Some(3));
        assert_eq!(ring.try_pop(), None);
    }

    #[test]
    fn buffer_fail() {
        let (sender, stream) = buffered::<(), String>(1);

        sender.fail("error".to_owned());

        assert_eq!(stream.collect().wait(), Err("error".to_owned()))
    }

    #[test]
    fn ring_async() {
        let ring: Arc<RingBuffer<i32, ()>> = Arc::new(RingBuffer::new(3));
        let clone = ring.clone();

        thread::spawn(move || {
            for _ in 0..10 {
                thread::sleep(Duration::from_millis(1));

                assert_eq!(clone.try_push(1), None);
            }
        });

        let mut count = 0;

        loop {
            if ring.try_pop().is_some() {
                count += 1;
            }

            if count == 10 {
                break;
            }
        }
    }

    #[test]
    fn sender_stream() {
        let (sender, stream) = buffered::<i32, ()>(3);

        thread::spawn(move || {
            thread::sleep(Duration::from_millis(10));

            for _ in 0..100 {
                sender.send(1);
            }
        });

        let sum = stream.fold(0, |sum, value| {
            Ok(sum + value)
        }).wait().unwrap();

        assert_eq!(sum, 100);
    }
}