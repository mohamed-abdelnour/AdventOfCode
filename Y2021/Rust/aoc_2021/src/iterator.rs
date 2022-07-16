//! A module providing iterators.

pub mod adjacent;

/// A wrapper around an array that implements `FromIterator<T> where T: Copy + Default`;
#[derive(Debug)]
pub struct Array<T, const N: usize>(pub [T; N]);

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

        let Array(right) = array
            .iter()
            .map(|s| s.parse())
            .collect::<Result<Array<usize, 2>, _>>()
            .unwrap();

        assert_eq!([0, 1], right);
        assert_eq!(left, right);
    }
}
