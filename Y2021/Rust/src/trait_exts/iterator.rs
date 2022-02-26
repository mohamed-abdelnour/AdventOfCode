use std::str::FromStr;

/// An extension trait to `std::iter::Iterator`.
pub trait IteratorExt: Sized + Iterator {
    /// Tries to parse an iterator over `&str` into a `Vec<F>` where `F: FromStr`.
    fn try_parse<'a, F>(self) -> Result<Vec<F>, F::Err>
    where
        Self: Iterator<Item = &'a str>,
        F: FromStr,
    {
        self.map(str::parse).collect()
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
