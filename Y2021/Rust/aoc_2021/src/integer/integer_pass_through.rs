use std::num::TryFromIntError;

use super::IntegerMarker;

pub trait IntegerPassThrough: IntegerMarker {
    const BITS: u32;
    const MAX: Self;
    const MIN: Self;
    const ONE: Self;
    const ZERO: Self;

    fn checked_add(self, rhs: Self) -> Option<Self>;
    fn checked_sub(self, rhs: Self) -> Option<Self>;
    fn leading_ones(self) -> u32;
    fn leading_zeros(self) -> u32;
    fn rem_euclid(self, rhs: Self) -> Self;

    fn try_cast<T: TryInto<Self>>(t: T) -> Result<Self, TryFromIntError> {
        t.try_into().map_err(|_| u8::try_from(-1_i8).unwrap_err())
    }
}

macro_rules! pass {
    (<$ty:ty>::$fn:ident(self $(, $arg:ident: $arg_ty:ty)*) -> $return:ty) => {
        fn $fn(self $(, $arg: $ty)*) -> $return {
            <$ty>::$fn(self $(, $arg)*)
        }
    };
}

macro_rules! impl_integer_pass_through {
    ($ty:ident) => {
        impl IntegerPassThrough for $ty {
            const BITS: u32 = Self::BITS;
            const MAX: Self = Self::MAX;
            const MIN: Self = Self::MIN;
            const ONE: Self = 1;
            const ZERO: Self = 0;

            pass!(<$ty>::checked_add(self, rhs: Self) -> Option<Self>);
            pass!(<$ty>::checked_sub(self, rhs: Self) -> Option<Self>);
            pass!(<$ty>::leading_ones(self) -> u32);
            pass!(<$ty>::leading_zeros(self) -> u32);
            pass!(<$ty>::rem_euclid(self, rhs: Self) -> Self);
        }

        #[cfg(test)]
        mod $ty {
            use crate::integer::Integer;

            use super::*;

            fn f<N: Integer>(n: N) -> N {
                n + N::try_cast(1_u128).unwrap()
            }

            #[test]
            fn cast() {
                let n: $ty = 1;
                assert_eq!(f(n), $ty::ONE + $ty::ONE);
            }
        }
    };
}

super::repeat_macro_for_integral!(impl_integer_pass_through);

#[cfg(test)]
mod tests {
    use utils::DisplayPanic;

    use super::*;

    #[test]
    #[should_panic(expected = "out of range integral type conversion attempted")]
    fn cast_panic() {
        <u8>::try_cast(-1_i8).unwrap_or_display_panic();
    }
}
