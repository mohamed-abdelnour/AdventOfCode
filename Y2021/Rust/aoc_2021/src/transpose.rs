use crate::bits::Bits;

/// An interface for transposing arrays of bits.
pub trait ArrayBitTranspose<T> {
    /// Transpose an array of bits.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use aoc_2021::transpose::ArrayBitTranspose;
    ///
    /// assert_eq!([5, 6, 7, 4].bit_transpose::<3>(), [10, 6, 15]);
    /// assert_eq!([12, 10].bit_transpose::<4>(), [0, 1, 2, 3]);
    /// ```
    ///
    /// - `[12, 10]`
    ///
    /// | Dec  | `3` | `2` | `1` | `0` |
    /// | ---- | --- | --- | --- | --- |
    /// | `12` | `1` | `1` | `0` | `0` |
    /// | `10` | `1` | `0` | `1` | `0` |
    ///
    /// - `[12, 10].bit_transpose::<4>()`
    ///
    /// | Col | `2` | `1` | Dec |
    /// | --- | --- | --- | --- |
    /// | `0` | `0` | `0` | `0` |
    /// | `1` | `0` | `1` | `1` |
    /// | `2` | `1` | `0` | `2` |
    /// | `3` | `1` | `1` | `3` |
    fn bit_transpose<const N: usize>(self) -> [T; N];
}

impl ArrayBitTranspose<usize> for &[usize] {
    fn bit_transpose<const N: usize>(self) -> [usize; N] {
        let mut r: [usize; N] = [usize::default(); N];
        self.iter().for_each(|n| {
            r.iter_mut().enumerate().for_each(|(i, row)| {
                *row <<= 1;
                *row += usize::from(n.bit(i));
            });
        });
        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_tranpose() {
        assert_eq!([5, 6, 7, 4].bit_transpose::<3>(), [10, 6, 15]);
        assert_eq!([12, 10].bit_transpose::<4>(), [0, 1, 2, 3]);
    }
}
