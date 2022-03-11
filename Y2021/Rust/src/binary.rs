use std::ops::Not;

use crate::define_error;

define_error!(ParseBinError, r#"a binary digit must be either "0" or "1""#);

/// The possible values of a binary digit.
#[derive(Debug, Eq, PartialEq)]
pub enum BinaryDigit {
    /// 0.
    Zero,
    /// 1.
    One,
}

impl From<bool> for BinaryDigit {
    fn from(p: bool) -> Self {
        match p {
            false => BinaryDigit::Zero,
            true => BinaryDigit::One,
        }
    }
}

impl From<BinaryDigit> for bool {
    fn from(d: BinaryDigit) -> Self {
        match d {
            BinaryDigit::Zero => false,
            BinaryDigit::One => true,
        }
    }
}

impl Not for BinaryDigit {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            BinaryDigit::Zero => BinaryDigit::One,
            BinaryDigit::One => BinaryDigit::Zero,
        }
    }
}

/// An interface for binary operations on numbers.
pub trait Bin {
    /// Returns the digit at index.
    fn digit(self, index: Self) -> BinaryDigit;
}

macro_rules! impl_bin {
    ($type:ty) => {
        impl Bin for $type {
            fn digit(self, index: Self) -> BinaryDigit {
                // For all n (such that n is an integer): n & 1 returns either 0 or 1; thus, it is
                // fine to unwrap the result.
                ((self >> index) & 1).try_into().unwrap()
            }
        }

        impl TryFrom<$type> for BinaryDigit {
            type Error = ParseBinError;

            fn try_from(digit: $type) -> Result<Self, Self::Error> {
                match digit {
                    0 => Ok(BinaryDigit::Zero),
                    1 => Ok(BinaryDigit::One),
                    _ => Err(ParseBinError),
                }
            }
        }

        impl From<BinaryDigit> for $type {
            fn from(digit: BinaryDigit) -> Self {
                match digit {
                    BinaryDigit::Zero => 0,
                    BinaryDigit::One => 1,
                }
            }
        }
    };
}

crate::repeat_macro!(impl_bin for u32);

#[cfg(test)]
mod tests {
    use test_utils::DisplayPanic;

    use super::*;

    #[test]
    fn bin_try_from_success() {
        let zero = BinaryDigit::try_from(0).unwrap();
        assert_eq!(zero, BinaryDigit::Zero);

        let one: BinaryDigit = 1.try_into().unwrap();
        assert_eq!(one, BinaryDigit::One);
    }

    #[test]
    #[should_panic(expected = r#"a binary digit must be either "0" or "1""#)]
    fn bin_try_from_fail() {
        BinaryDigit::try_from(2).display_panic();
    }

    #[test]
    fn bin_digit() {
        (0u32..32).for_each(|i| {
            let n = 2u32.pow(i);
            assert_eq!(n.digit(i), BinaryDigit::One);
            (0u32..32).filter(|j| j != &i).for_each(|j| {
                assert_eq!(n.digit(j), BinaryDigit::Zero);
            });
        });
        let (even, odd): (Vec<u32>, Vec<u32>) = (0..1000).partition(|n| n % 2 == 0);
        let check_first_bit = |vec: Vec<u32>, expected| {
            vec.into_iter()
                .for_each(|n| assert_eq!(n.digit(0), expected));
        };
        check_first_bit(even, BinaryDigit::Zero);
        check_first_bit(odd, BinaryDigit::One);
    }

    #[test]
    fn bin_bool() {
        let f: bool = bool::from(BinaryDigit::Zero);
        let t: bool = bool::from(BinaryDigit::One);
        assert!(!f);
        assert!(t);

        let zero: BinaryDigit = BinaryDigit::from(false);
        let one: BinaryDigit = BinaryDigit::from(true);
        assert_eq!(BinaryDigit::Zero, zero);
        assert_eq!(BinaryDigit::One, one);
    }

    #[test]
    fn bin_not() {
        assert_eq!(!BinaryDigit::Zero, BinaryDigit::One);
        assert_eq!(!BinaryDigit::One, BinaryDigit::Zero);
    }
}
