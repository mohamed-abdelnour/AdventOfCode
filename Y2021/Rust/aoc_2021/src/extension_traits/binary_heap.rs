//! Extension to `std::collections::BinaryHeap`.

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

/// An extension trait that adds functionality to `std::collections::BinaryHeap`.
pub trait BinaryHeapExt<T> {
    /// Returns an iterator over the heap that retrieves elements in heap order.
    fn iter_heap(self) -> BinaryHeapIter<T>;
}

impl<T: Ord> BinaryHeapExt<T> for BinaryHeap<T> {
    fn iter_heap(self) -> BinaryHeapIter<T> {
        BinaryHeapIter { heap: self }
    }
}

#[cfg(test)]
mod tests {
    use crate::iterator::Array;

    use super::*;

    #[test]
    fn heap() {
        const MAX: usize = 10;

        let Array(array) = (0..MAX).collect::<Array<_, MAX>>();
        let heap = BinaryHeap::from(array);
        heap.iter_heap().enumerate().for_each(|(i, v)| {
            assert_eq!(MAX - 1 - i, v);
        })
    }
}
