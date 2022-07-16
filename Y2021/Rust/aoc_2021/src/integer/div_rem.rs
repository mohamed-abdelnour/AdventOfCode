//! A convenience module for getting both `std::ops::Div` and `std::ops::Rem`.

use super::Integer;

#[allow(missing_docs)]
pub struct DivRem<N> {
    pub div: N,
    pub rem: N,
}

impl<N: Integer> DivRem<N> {
    /// Returns a new `DivRem<N>` with `div: p / q` and `rem: p % q`.
    pub fn new(p: N, q: N) -> Self {
        Self {
            div: p / q,
            rem: p % q,
        }
    }
}
