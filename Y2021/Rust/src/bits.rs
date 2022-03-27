use std::ops::Not;

use crate::define_error;

define_error!(ParseBinError, r#"a bit must be either "0" or "1""#);

/// The possible values of a bit.
#[allow(missing_docs)]
#[derive(Debug, Eq, PartialEq)]
pub enum Bit {
    Zero,
    One,
}

impl From<bool> for Bit {
    fn from(p: bool) -> Self {
        match p {
            false => Bit::Zero,
            true => Bit::One,
        }
    }
}

impl From<Bit> for bool {
    fn from(d: Bit) -> Self {
        match d {
            Bit::Zero => false,
            Bit::One => true,
        }
    }
}

impl Not for Bit {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Bit::Zero => Bit::One,
            Bit::One => Bit::Zero,
        }
    }
}

/// An interface for binary operations on numbers.
pub trait Bits {
    /// Returns the bit at index.
    fn bit(self, index: Self) -> Bit;
}

macro_rules! impl_bin {
    ($type:ty) => {
        impl Bits for $type {
            fn bit(self, index: Self) -> Bit {
                // For all n (such that n is an integer): n & 1 returns either 0 or 1; thus, it is
                // fine to unwrap the result.
                ((self >> index) & 1).try_into().unwrap()
            }
        }

        impl TryFrom<$type> for Bit {
            type Error = ParseBinError;

            fn try_from(bit: $type) -> Result<Self, Self::Error> {
                match bit {
                    0 => Ok(Bit::Zero),
                    1 => Ok(Bit::One),
                    _ => Err(ParseBinError),
                }
            }
        }

        impl From<Bit> for $type {
            fn from(bit: Bit) -> Self {
                bit as $type
            }
        }
    };
}

crate::repeat_macro!(impl_bin for u32 usize);

#[cfg(test)]
mod tests {
    use test_utils::DisplayPanic;

    use super::*;

    #[test]
    fn bin_try_from_success() {
        let zero = Bit::try_from(0_usize).unwrap();
        assert_eq!(zero, Bit::Zero);

        let one: Bit = 1_usize.try_into().unwrap();
        assert_eq!(one, Bit::One);
    }

    #[test]
    #[should_panic(expected = r#"a bit must be either "0" or "1""#)]
    fn bin_try_from_fail() {
        Bit::try_from(2_usize).display_panic();
    }

    #[test]
    fn bit() {
        (0u32..32).for_each(|i| {
            let n = 2u32.pow(i);
            assert_eq!(n.bit(i), Bit::One);
            (0u32..32).filter(|j| j != &i).for_each(|j| {
                assert_eq!(n.bit(j), Bit::Zero);
            });
        });
        let (even, odd): (Vec<u32>, Vec<u32>) = (0..1000).partition(|n| n % 2 == 0);
        let check_first_bit = |vec: Vec<u32>, expected| {
            vec.into_iter().for_each(|n| assert_eq!(n.bit(0), expected));
        };
        check_first_bit(even, Bit::Zero);
        check_first_bit(odd, Bit::One);
    }

    #[test]
    fn bin_bool() {
        let f: bool = bool::from(Bit::Zero);
        let t: bool = bool::from(Bit::One);
        assert!(!f);
        assert!(t);

        let zero: Bit = Bit::from(false);
        let one: Bit = Bit::from(true);
        assert_eq!(Bit::Zero, zero);
        assert_eq!(Bit::One, one);
    }

    #[test]
    fn bin_not() {
        assert_eq!(!Bit::Zero, Bit::One);
        assert_eq!(!Bit::One, Bit::Zero);
    }
}
