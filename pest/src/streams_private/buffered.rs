// pest. The Elegant Parser
// Copyright (C) 2017  Dragoș Tiselice
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::{Cell, UnsafeCell};
use std::{mem, ptr};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};

use futures::{Async, Poll};
use futures::stream::Stream;
use futures::task::{self, Task};

#[cfg(target_pointer_width = "32")]
const PAD: usize = 16;
#[cfg(target_pointer_width = "64")]
const PAD: usize = 8;


#[repr(C)]
struct RingBuffer<T, E> {
    read:          AtomicUsize,
    last_write:    Cell<usize>,
    _padding1:     [usize; PAD - 2],
    write:         AtomicUsize,
    last_read:     Cell<usize>,
    _padding2:     [usize; PAD - 2],
    buffer:        *mut T,
    capacity:      usize,
    size:          usize,
    pub error:     AtomicPtr<E>,
    pub is_closed: AtomicBool,
    task:          UnsafeCell<Option<Task>>,
    pub task_lock: AtomicBool
}

impl<T, E> RingBuffer<T, E> {
    #[inline]
    pub fn new(capacity: usize) -> RingBuffer<T, E> {
        let size = capacity.next_power_of_two();
        let mut vec = Vec::with_capacity(size);
        let ptr = vec.as_mut_ptr();

        mem::forget(vec);

        RingBuffer {
            read:       AtomicUsize::new(0),
            last_write: Cell::new(0),
            _padding1:  [0; PAD - 2],
            write:      AtomicUsize::new(0),
            last_read:  Cell::new(0),
            _padding2:  [0; PAD - 2],
            buffer:     ptr,
            capacity:   capacity,
            size:       size,
            error:      AtomicPtr::new(ptr::null_mut()),
            is_closed:  AtomicBool::new(false),
            task:       UnsafeCell::new(None),
            task_lock:  AtomicBool::new(false)
        }
    }

    #[inline]
    pub fn try_push(&self, value: T) -> Option<T> {
        let read = self.last_read.get();
        let write = self.write.load(Ordering::Relaxed);

        if read + self.capacity <= write {
            let read = self.read.load(Ordering::Relaxed);
            self.last_read.set(read);

            if read + self.capacity <= write {
                return Some(value);
            }
        }

        self.push(write, value);

        self.write.store(write.wrapping_add(1), Ordering::Relaxed);

        self.try_unpark();

        None
    }

    #[inline]
    pub fn try_pop(&self) -> Option<T> {
        let read = self.read.load(Ordering::Relaxed);
        let write = self.last_write.get();

        if read == write {
            let write = self.write.load(Ordering::Relaxed);
            self.last_write.set(write);

            if read == write {
                return None;
            }
        }

        let result = self.pop(read);

        self.read.store(read.wrapping_add(1), Ordering::Relaxed);

        Some(result)
    }

    #[inline]
    pub fn try_park(&self) -> bool {
        let locked = self.task_lock.swap(true, Ordering::Relaxed);

        if !locked {
            self.swap(Some(task::park()));

            self.task_lock.store(false, Ordering::Relaxed);
        }

        !locked
    }

    #[inline]
    pub fn try_unpark(&self) -> bool {
        let locked = self.task_lock.swap(true, Ordering::Relaxed);

        if !locked {
            let task = self.swap(None);

            if let Some(task) = task {
                task.unpark();
            }

            self.task_lock.store(false, Ordering::Relaxed);
        }

        !locked
    }

    #[inline]
    pub fn swap(&self, option: Option<Task>) -> Option<Task> {
        let mut task = unsafe { &mut *self.task.get()};
        mem::replace(task, option)
    }

    #[inline]
    fn push(&self, index: usize, value: T) {
        unsafe {
            ptr::write(&mut *self.buffer.offset(self.index(index)), value);
        }
    }

    #[inline]
    fn pop(&self, index: usize) -> T {
        unsafe {
            ptr::read(self.buffer.offset(self.index(index)))
        }
    }

    #[inline]
    fn index(&self, index: usize) -> isize {
        (index & (self.size - 1)) as isize
    }
}

impl<T, E> Drop for RingBuffer<T, E> {
    fn drop(&mut self) {
        unsafe  {
            Vec::from_raw_parts(self.buffer, 0, self.size);
        }
    }
}

unsafe impl<T: Send, E: Send> Send for RingBuffer<T, E> {}
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
                    loop {
                        if self.buffer.try_park() {
                            break;
                        }
                    }

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
    pub fn fail(self, error: E) {
        let boxed = Box::new(error);
        self.buffer.error.store(Box::into_raw(boxed), Ordering::Release);
    }
}

impl<T, E> Drop for BufferedSender<T, E> {
    fn drop(&mut self) {
        self.buffer.is_closed.store(true, Ordering::Relaxed);

        loop {
            if self.buffer.try_unpark() {
                break;
            }
        }
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
        let ring: Arc<RingBuffer<i32, ()>> = Arc::new(RingBuffer::new(10));
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
