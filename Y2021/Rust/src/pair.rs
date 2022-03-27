use std::ops::{Add, Mul, Sub};
use std::str::FromStr;

/// A generic, homogeneous pair type.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Pair<T>(pub T, pub T);

impl<T> Pair<T> {
    /// Multiplies the two elements in the pair.
    pub fn prod(&self) -> T::Output
    where
        T: Copy + Mul,
    {
        let &Pair(a, b) = self;
        a * b
    }
}

impl<T: FromStr> TryFrom<(&str, &str)> for Pair<T> {
    type Error = T::Err;

    fn try_from((x, y): (&str, &str)) -> Result<Self, Self::Error> {
        let x = x.parse()?;
        let y = y.parse()?;
        Ok(Self(x, y))
    }
}

impl<T> From<Pair<T>> for (T, T) {
    fn from(Pair(x, y): Pair<T>) -> Self {
        (x, y)
    }
}

impl<T> From<Pair<T>> for [T; 2] {
    fn from(Pair(x, y): Pair<T>) -> Self {
        [x, y]
    }
}

impl<T: Add> Add for Pair<T> {
    type Output = Pair<T::Output>;

    fn add(self, Self(m, n): Self) -> Self::Output {
        let Pair(x, y) = self;
        Pair(x + m, y + n)
    }
}

impl<T: Sub> Sub for Pair<T> {
    type Output = Pair<T::Output>;

    fn sub(self, Self(m, n): Self) -> Self::Output {
        let Pair(x, y) = self;
        Pair(x - m, y - n)
    }
}
