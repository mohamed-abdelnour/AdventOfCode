use super::IntegerMarker;

pub trait IntegerPassThrough: IntegerMarker {
    const MIN: Self;
    const MAX: Self;
    const ZERO: Self;
    const ONE: Self;

    fn checked_add(self, rhs: Self) -> Option<Self>;

    fn checked_sub(self, rhs: Self) -> Option<Self>;
}

macro_rules! impl_integer_pass_through {
    ($type:ty) => {
        impl IntegerPassThrough for $type {
            const MIN: Self = Self::MIN;
            const MAX: Self = Self::MAX;
            const ZERO: Self = 0;
            const ONE: Self = 1;

            fn checked_add(self, rhs: Self) -> Option<Self> {
                <$type>::checked_add(self, rhs)
            }

            fn checked_sub(self, rhs: Self) -> Option<Self> {
                <$type>::checked_sub(self, rhs)
            }
        }
    };
}

super::repeat_macro_for_integral!(impl_integer_pass_through);
