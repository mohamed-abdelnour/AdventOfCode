use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};
use std::str::FromStr;

use crate::repeat_macro;

/// A generic, homogeneous pair type.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Pair<T>(pub T, pub T);

impl<T> Pair<T> {
    fn pointwise<U, F>(self, Self(m, n): Self, f: F) -> Pair<U>
    where
        F: Fn(T, T) -> U,
    {
        Pair(f(self.0, m), f(self.1, n))
    }

    fn pointwise_mut<F>(&mut self, Self(m, n): Self, mut f: F)
    where
        F: FnMut(&mut T, T),
    {
        f(&mut self.0, m);
        f(&mut self.1, n);
    }

    fn fold<U, F>(self, f: F) -> U
    where
        F: FnOnce(T, T) -> U,
    {
        f(self.0, self.1)
    }

    /// Multiplies the two elements in the pair.
    pub fn product(self) -> T::Output
    where
        T: Mul,
    {
        self.fold(Mul::mul)
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

macro_rules! impl_ops {
    (($trait: ident::$function: ident)) => {
        impl<T: $trait> $trait for Pair<T> {
            type Output = Pair<T::Output>;

            fn $function(self, rhs: Self) -> Self::Output {
                self.pointwise(rhs, $trait::$function)
            }
        }
    };
}

macro_rules! impl_ops_assign {
    (($trait: ident::$function: ident)) => {
        impl<T: $trait> $trait for Pair<T> {
            fn $function(&mut self, rhs: Self) {
                self.pointwise_mut(rhs, $trait::$function);
            }
        }
    };
}

repeat_macro! {
    impl_ops for
        (Add::add)
        (Sub::sub)
}

repeat_macro! {
    impl_ops_assign for
        (AddAssign::add_assign)
        (SubAssign::sub_assign)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn product() {
        assert_eq!(Pair(2, 7).product(), 14);
    }

    #[test]
    fn ops() {
        let p = Pair(2, 3);
        let q = Pair(0, 1);

        let add = p + q;
        let mut add_assign = p;
        add_assign += q;

        let sub = p - q;
        let mut sub_assign = p;
        sub_assign -= q;

        assert_eq!(add, Pair(2, 4));
        assert_eq!(add_assign, add);

        assert_eq!(sub, Pair(2, 2));
        assert_eq!(sub_assign, sub);
    }
}
