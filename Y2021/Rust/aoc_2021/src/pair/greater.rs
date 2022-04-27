use super::Pair;

/// An interface for finding the greatest of two pairs.
pub trait Greater {
    /// Returns whether or not this point is greater than another one.
    fn gt(&self, other: &Self) -> bool;
}

impl<T: PartialOrd> Greater for Pair<T> {
    fn gt(&self, other: &Self) -> bool {
        self.0 > other.0 && self.1 > other.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Point = Pair<u8>;

    #[test]
    fn is_gt_than() {
        let a: Point = Pair(0, 0);
        let b: Point = Pair(0, 1);
        let c: Point = Pair(1, 0);
        let d: Point = Pair(1, 1);

        assert!(!a.gt(&a));
        assert!(!b.gt(&a));
        assert!(!c.gt(&a));
        assert!(d.gt(&a));
    }
}
