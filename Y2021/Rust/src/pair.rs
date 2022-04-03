use std::iter;
use std::ops::{Add, AddAssign, Sub, SubAssign};

use crate::integer::Integer;
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

    /// Returns an iterator over the pairs adjacent to this one.
    pub fn adjacent(self) -> impl Iterator<Item = Self>
    where
        T: Integer,
    {
        let vertical = self.0.adjacent().zip(iter::repeat(self.1));
        let horizontal = iter::repeat(self.0).zip(self.1.adjacent());

        vertical.chain(horizontal).map(Into::into)
    }
}

impl<T> From<Pair<T>> for (T, T) {
    fn from(Pair(x, y): Pair<T>) -> Self {
        (x, y)
    }
}

impl<T> From<(T, T)> for Pair<T> {
    fn from((x, y): (T, T)) -> Self {
        Pair(x, y)
    }
}

impl<T> From<Pair<T>> for [T; 2] {
    fn from(Pair(x, y): Pair<T>) -> Self {
        [x, y]
    }
}

impl<T> IntoIterator for Pair<T> {
    type Item = T;
    type IntoIter = <[T; 2] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        <[T; 2]>::from(self).into_iter()
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
        assert_eq!(Pair(2, 7).into_iter().product::<u8>(), 14);
    }

    #[test]
    fn adjacent() {
        let adjacent = Pair(2_usize, 3).adjacent().collect::<Vec<_>>();
        assert_eq!(adjacent, [Pair(1, 3), Pair(3, 3), Pair(2, 2), Pair(2, 4),]);
    }

    #[test]
    fn adjacent_overflow() {
        let adjacent = Pair(0_usize, 0).adjacent().collect::<Vec<_>>();
        assert_eq!(adjacent, [Pair(1, 0), Pair(0, 1)]);
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
