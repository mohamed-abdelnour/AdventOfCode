use std::num::ParseIntError;

/// A trait for types that implement `from_str_radix`.
pub trait FromStrRadix: Sized {
    /// Wraps the type's `from_str_radix`.
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>;
}

macro_rules! impl_num {
    ($type:ty) => {
        impl FromStrRadix for $type {
            fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> {
                Self::from_str_radix(src, radix)
            }
        }
    };
}

crate::repeat_macro!(impl_num for u32);
