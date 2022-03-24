use std::cmp::Ordering;

/// An interface for searching over types.
pub trait Search {
    /// The type used for indexing.
    type Index;

    /// Searches with a comparator function.
    fn search_by<F>(self, f: F) -> Result<Self::Index, Self::Index>
    where
        F: FnMut(Self::Index) -> Ordering;
}

/// A type that implements binary search.
pub struct Binary {
    /// The first index.
    pub low: usize,
    /// The number of elements: the last index + 1.
    pub size: usize,
}

impl Binary {
    /// Constructs a new `Binary` with a given starting index and size.
    pub fn new(low: usize, size: usize) -> Self {
        Self { low, size }
    }
}

impl From<Binary> for (usize, usize) {
    fn from(Binary { low, size }: Binary) -> Self {
        (low, size)
    }
}

impl Search for Binary {
    type Index = usize;

    fn search_by<F>(self, mut f: F) -> Result<Self::Index, Self::Index>
    where
        F: FnMut(Self::Index) -> Ordering,
    {
        let (mut left, mut right) = self.into();
        while left < right {
            let mid = (left + right) / 2;

            match f(mid) {
                Ordering::Less => {
                    left = mid + 1;
                }
                Ordering::Greater => {
                    right = mid;
                }
                Ordering::Equal => {
                    return Ok(mid);
                }
            };
        }

        Err(left)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check<O: Ord>(slice: &[O], index: O) {
        let ours = Binary::new(0, slice.len()).search_by(|i| slice[i].cmp(&index));
        let theirs = slice.binary_search(&index);
        assert_eq!(ours, theirs);
    }

    #[test]
    fn binary_traverse() {
        (0..=8).for_each(|i| check(&[2, 4, 6], i));
        (0..=10).for_each(|i| check(&[2, 4, 6, 8], i));
    }
}
