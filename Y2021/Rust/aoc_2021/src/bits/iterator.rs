use std::cmp::Ordering;

use crate::integer::Integer;

use super::Bit;

/// An iterator over the bits in an integer, ordered from the least significant bit to the most
/// significant one.
#[derive(Debug, Default)]
pub struct BitIter<N> {
    n: N,
    length: u32,
}

impl<N: Integer> From<N> for BitIter<N> {
    fn from(n: N) -> Self {
        let length = match n.cmp(&N::ZERO) {
            Ordering::Greater => N::BITS - n.leading_zeros(),
            Ordering::Less => N::BITS - n.leading_ones() + 1,
            Ordering::Equal => 0,
        };

        BitIter { n, length }
    }
}

impl<N: Integer> BitIter<N> {
    /// Returns a new `BitIter`. The iterator is padded as needed to fill `length`.
    ///
    /// # Panics
    ///
    /// This function panics if `n` cannot be represented in `length` bits.
    pub fn new(n: N, length: u32) -> Self {
        let mut iter = BitIter::from(n);

        assert!(length >= iter.length);
        iter.length = length;

        iter
    }
}

impl<N: Integer> Iterator for BitIter<N> {
    type Item = Bit;

    fn next(&mut self) -> Option<Self::Item> {
        if self.length == 0 {
            None
        } else {
            self.length -= 1;

            // UNWRAP: a 2 can be represented by all integral types.
            //
            // The conversion to Bit is guaranteed to succeed; because for all n (where n is an
            // integer), n.rem_euclid(2) is either 0 or 1.
            //
            // Result::ok is used to wrap the success value in an Option::Some instead of a
            // Result::Ok.
            let lsb = self.n.rem_euclid(N::try_cast(2).unwrap()).try_into().ok();

            self.n >>= N::ONE;

            lsb
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // CAST: a u32 is guaranteed to fit in a usize.
        let length = self.length as usize;
        (length, Some(length))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn length() {
        assert_eq!(0_f64.log2() as u32, 0);

        (0..=i8::MAX).for_each(|n| {
            let length = (n as f64).log2() + 1.0;

            assert_eq!(BitIter::from(n).length, length.floor() as u32);
            assert_eq!(BitIter::from(-n).length, length.ceil() as u32);
        });
    }

    fn f<N: Integer>(n: N) -> Vec<u8> {
        BitIter::from(n).map(Into::into).collect()
    }

    fn g<N: Integer>(n: N, l: u32) -> Vec<u8> {
        BitIter::new(n, l).map(Into::into).collect()
    }

    #[test]
    fn bits() {
        assert_eq!(f(0), []);
        assert_eq!(g(0, 4), [0, 0, 0, 0]);

        assert_eq!(f(1), [1]);
        assert_eq!(g(1, 2), [1, 0]);

        assert_eq!(f(2), [0, 1]);
        assert_eq!(g(2, 2), [0, 1]);

        assert_eq!(f(13), [1, 0, 1, 1]);
        assert_eq!(g(13, 6), [1, 0, 1, 1, 0, 0]);

        assert_eq!(f(-0_i8), []);
        assert_eq!(g(-0_i8, 8), [0, 0, 0, 0, 0, 0, 0, 0]);

        assert_eq!(f(-1_i8), [1]);
        assert_eq!(g(-1_i8, 4), [1, 1, 1, 1]);

        assert_eq!(f(-2_i8), [0, 1]);
        assert_eq!(g(-2_i8, 2), [0, 1]);

        assert_eq!(f(-13_i8), [1, 1, 0, 0, 1]);
        assert_eq!(g(-13_i8, 10), [1, 1, 0, 0, 1, 1, 1, 1, 1, 1]);
    }
}
