use std::fmt::{Debug, Display};
use std::ops::{Add, AddAssign, BitAnd, Div, Rem, Shr, Sub};

pub trait IntegerMarker:
    Eq
    + Ord
    + PartialEq
    + PartialOrd
    + Default
    + Debug
    + Display
    + Copy
    + Sized
    + Add<Output = Self>
    + BitAnd<Output = Self>
    + Div<Output = Self>
    + Rem<Output = Self>
    + Shr<Output = Self>
    + Sub<Output = Self>
    + AddAssign
    + TryFrom<u8>
    + TryFrom<u16>
    + TryFrom<u32>
    + TryFrom<u64>
    + TryFrom<u128>
    + TryFrom<usize>
    + TryFrom<i8>
    + TryFrom<i16>
    + TryFrom<i32>
    + TryFrom<i64>
    + TryFrom<i128>
    + TryFrom<isize>
{
}

macro_rules! impl_integer_marker {
    ($type:ty) => {
        impl IntegerMarker for $type {}
    };
}

super::repeat_macro_for_integral!(impl_integer_marker);
