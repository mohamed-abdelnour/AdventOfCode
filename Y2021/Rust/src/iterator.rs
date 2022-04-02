use crate::newtype_into_inner;

/// A module providing an iterator over the two numbers adjacent to any integer.
pub mod adjacent;

/// A module providing an alternative iterator over `std::collections::BinaryHeap`;
pub mod binary_heap;

/// A wrapper around an array that implements `FromIterator<T> where T: Copy + Default`;
#[derive(Debug)]
pub struct Array<T, const N: usize>([T; N]);

impl<T, const N: usize> Array<T, N> {
    newtype_into_inner!([T; N]);
}

impl<const N: usize, T> FromIterator<T> for Array<T, N>
where
    T: Copy + Default,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut r = [Default::default(); N];
        iter.into_iter()
            .enumerate()
            .for_each(|(i, item)| r[i] = item);
        Self(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array() {
        let array = ["0", "1"];

        let left = array
            .iter()
            .map(|s| s.parse())
            .collect::<Result<Vec<usize>, _>>()
            .unwrap();

        let right: [usize; 2] = array
            .iter()
            .map(|s| s.parse())
            .collect::<Result<Array<usize, 2>, _>>()
            .unwrap()
            .into_inner();

        assert_eq!([0, 1], right);
        assert_eq!(left, right);
    }
}
