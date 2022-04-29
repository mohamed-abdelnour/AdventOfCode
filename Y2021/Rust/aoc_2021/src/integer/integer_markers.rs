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
{
}

macro_rules! impl_integer_marker {
    ($type:ty) => {
        impl IntegerMarker for $type {}
    };
}

super::repeat_macro_for_integral!(impl_integer_marker);
