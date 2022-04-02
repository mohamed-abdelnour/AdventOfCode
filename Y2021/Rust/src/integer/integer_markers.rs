use std::fmt::Debug;
use std::ops::{Add, BitAnd, Shr, Sub};

pub trait IntegerMarker:
    Eq
    + Ord
    + PartialEq
    + PartialOrd
    + Default
    + Debug
    + Copy
    + Sized
    + Add<Output = Self>
    + BitAnd<Output = Self>
    + Shr<Output = Self>
    + Sub<Output = Self>
{
}

macro_rules! impl_integer_marker {
    ($type:ty) => {
        impl IntegerMarker for $type {}
    };
}

super::repeat_macro_for_integral!(impl_integer_marker);
