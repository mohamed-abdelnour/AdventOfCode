//! A module that defines a generic, homogeneous pair type.

use std::{
    iter,
    ops::{Add, AddAssign, Sub, SubAssign},
};

use crate::{integer::Integer, repeat_macro};

pub mod greater;

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
}

impl<N: Integer> Pair<N> {
    /// Returns an iterator over the pairs adjacent to this one in the four cardinal directions.
    pub fn adjacent_cardinal(self) -> impl Iterator<Item = Self> {
        let vertical = self.0.adjacent().zip(iter::repeat(self.1));
        let horizontal = iter::repeat(self.0).zip(self.1.adjacent());
        vertical.chain(horizontal).map(Pair::from)
    }

    /// Returns an iterator over the pairs adjacent to this one in the four ordinal directions.
    fn adjacent_ordinal(self) -> impl Iterator<Item = Self> {
        self.0
            .adjacent()
            .flat_map(move |row| iter::repeat(row).zip(self.1.adjacent()).map(Pair::from))
    }

    /// Returns an iterator over the pairs adjacent to this one.
    pub fn adjacent(self) -> impl Iterator<Item = Self> {
        self.adjacent_cardinal().chain(self.adjacent_ordinal())
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
    fn adjacent_cardinal() {
        let cardinals = Pair(2_usize, 3).adjacent_cardinal().collect::<Vec<_>>();
        assert_eq!(cardinals, [Pair(1, 3), Pair(3, 3), Pair(2, 2), Pair(2, 4),]);
    }

    #[test]
    fn adjacent_cardinal_overflow() {
        let cardinals = Pair(0_usize, 0).adjacent_cardinal().collect::<Vec<_>>();
        assert_eq!(cardinals, [Pair(1, 0), Pair(0, 1)]);
    }

    #[test]
    fn adjacent_ordinal() {
        let ordinals = Pair(2_usize, 3).adjacent_ordinal().collect::<Vec<_>>();
        assert_eq!(ordinals, [Pair(1, 2), Pair(1, 4), Pair(3, 2), Pair(3, 4),]);
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
