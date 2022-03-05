use std::str::FromStr;

use crate::num::FromStrRadix;

/// An private extension trait to `std::iter::Iterator`.
trait PrivateIteratorExt: Sized + Iterator {
    /// Tries to map `f` over an iterator.
    fn try_map<B, F, T, E>(self, f: F) -> Result<T, E>
    where
        F: Fn(Self::Item) -> Result<B, E>,
        T: FromIterator<B>,
    {
        self.map(f).collect()
    }
}

impl<I: Iterator> PrivateIteratorExt for I {}

/// An extension trait to `std::iter::Iterator`.
pub trait IteratorExt: Sized + Iterator {
    /// Tries to parse an iterator over `&str` into a `Vec<F>` where `F: FromStr`.
    fn try_parse<'a, F>(self) -> Result<Vec<F>, F::Err>
    where
        Self: Iterator<Item = &'a str>,
        F: FromStr,
    {
        self.try_map(str::parse)
    }

    /// Tries to parse an iterator over `&str` into a `Vec<N>` where `F: FromStrRadix`.
    fn try_parse_radix<'a, N>(self, radix: u32) -> Result<Vec<N>, std::num::ParseIntError>
    where
        Self: Iterator<Item = &'a str>,
        N: FromStrRadix,
    {
        self.try_map(|s| N::from_str_radix(s, radix))
    }
}

impl<I: Iterator> IteratorExt for I {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_parse() {
        let intersperse_line_feed = |mut acc: String, c| {
            acc.push(c);
            acc.push('\n');
            acc
        };
        let parsed = ('0'..='9')
            .fold(String::from(""), intersperse_line_feed)
            .lines()
            .try_parse::<u8>()
            .unwrap();
        let expected = (0..=9).collect::<Vec<_>>();
        assert_eq!(parsed, expected);
    }
}
