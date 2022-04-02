use std::collections::BinaryHeap;

/// An iterator over `std::collections::BinaryHeap` that returns elements in heap order.
pub struct BinaryHeapIter<T> {
    heap: BinaryHeap<T>,
}

impl<T: Ord> Iterator for BinaryHeapIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.heap.pop()
    }
}

impl<T> From<BinaryHeap<T>> for BinaryHeapIter<T> {
    fn from(heap: BinaryHeap<T>) -> Self {
        BinaryHeapIter { heap }
    }
}

/// An extension trait that add functionality to `std::collections::BinaryHeap`.
pub trait BinaryHeapExt<T> {
    /// Returns an iterator over the heap that retrieves elements in heap order.
    fn iter_heap(self) -> BinaryHeapIter<T>;
}

impl<T: Ord> BinaryHeapExt<T> for BinaryHeap<T> {
    fn iter_heap(self) -> BinaryHeapIter<T> {
        self.into()
    }
}
