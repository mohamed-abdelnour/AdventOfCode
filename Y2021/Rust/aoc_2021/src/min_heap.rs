use std::{cmp::Reverse, collections::BinaryHeap};

/// A wrapper around `std::collections::BinaryHeap` that "turns" it into a min-heap.
#[derive(Debug, Default)]
pub struct MinHeap<T: Ord> {
    heap: BinaryHeap<Reverse<T>>,
}

impl<T: Ord> MinHeap<T> {
    /// Removes the smallest item from the heap and returns it, or returns `None` if it is empty.
    pub fn pop(&mut self) -> Option<T> {
        self.heap.pop().map(|Reverse(item)| item)
    }

    /// Pushes an item onto the binary heap.
    pub fn push(&mut self, item: T) {
        self.heap.push(Reverse(item));
    }
}
