use std::cell::UnsafeCell;
use std::ptr;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};

use futures::{Async, Poll};
use futures::stream::Stream;
use futures::task::{park, Task};

struct RingBuffer<T> {
    buffer:        UnsafeCell<Vec<Option<T>>>,
    read:          AtomicUsize,
    write:         AtomicUsize,
    pub is_closed: AtomicBool,
    pub task:      AtomicPtr<Task>,
    capacity:      usize,
    size:          usize
}

impl<T> RingBuffer<T> {
    #[inline]
    pub fn new(capacity: usize) -> RingBuffer<T> {
        let size = capacity.next_power_of_two();

        RingBuffer {
            buffer:    UnsafeCell::new(Vec::with_capacity(size)),
            read:      AtomicUsize::new(0),
            write:     AtomicUsize::new(0),
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
            let mut buffer = unsafe { &mut *self.buffer.get() };

            if buffer.len() < buffer.capacity() {
                buffer.push(Some(value));
            } else {
                buffer[self.index(write)] = Some(value);
            }

            self.write.store(write.wrapping_add(1), Ordering::Relaxed);

            self.try_unpark();

            None
        } else {
            Some(value)
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
            unsafe { (*task).unpark(); }
        }

    }

    #[inline]
    fn index(&self, pos: usize) -> usize {
        pos & (self.size - 1)
    }
}

unsafe impl<T: Sync> Sync for RingBuffer<T> {}

pub struct BufferedStream<T> {
    buffer: Arc<RingBuffer<T>>
}

impl<T> Stream for BufferedStream<T> {
    type Item  = T;
    type Error = ();

    fn poll(&mut self) -> Poll<Option<T>, ()> {
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

pub struct BufferedSender<T> {
    buffer: Arc<RingBuffer<T>>
}

impl<T> BufferedSender<T> {
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
}

impl<T> Drop for BufferedSender<T> {
    fn drop(&mut self) {
        self.buffer.try_unpark();

        self.buffer.is_closed.store(true, Ordering::Relaxed);
    }
}

pub fn buffered<T>(capacity: usize) -> (BufferedSender<T>, BufferedStream<T>) {
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
        let ring = RingBuffer::new(3);

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
    fn ring_async() {
        let ring = Arc::new(RingBuffer::new(3));
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
        let (sender, stream) = buffered::<i32>(3);

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
