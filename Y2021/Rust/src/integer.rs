use std::mem;

use crate::iterator::adjacent::Adjacent;

mod integer_markers;
use integer_markers::IntegerMarker;

mod integer_pass_through;
use integer_pass_through::IntegerPassThrough;

/// An interface for integral types.
pub trait Integer: IntegerMarker + IntegerPassThrough {
    /// Returns an iterator over the two numbers adjacent to `self`.
    fn adjacent(self) -> Adjacent<Self> {
        self.into()
    }

    /// Calculates the greatest common divisor of two integers: `self` and `other`.
    //
    // This uses Stein's algorithm to compute the GCD. A few identities are used to reduce the
    // problem of finding the GCD:
    //
    // 1. `gcd(m, n) = gcd(n, m)`:
    //
    //    `gcd` is commutative.
    //
    // 2. `gcd(p, 0) = 0`:
    //
    //    Any number divides zero.
    //
    // 3. `gcd(m, n) = 2 * gcd(a, b)`, where `m = 2 * a` and `n = 2 * b`:
    //
    //    If both `m` and `n` are even, they are guaranteed to have a common factor of 2. In
    //    practice, this is optimised by getting the greatest common factor that is a power of 2.
    //
    //    Let `a` and `b` be two odd integers such that:
    //
    //    - `m = (2 ^ i) * a`,
    //    - `n = (2 ^ j) * b` and
    //    - `k = min(i, j)` where `i` and `j` are integers, then
    //    - `gcd(m, n) =  (2 ^ k) * gcd(a, b)`.
    //
    // 4. `gcd(m, n) = gcd(a, n)`, where `m = 2 * a` and `n = 2 * b + 1`:
    //
    //    If `m` is even and `n` is odd, they are guaranteed to not have a common factor of 2.
    //    Again, this is optimised by getting the greatest factor of `m` that is a power of 2.
    //
    //    Let `c` be an odd integer such that:
    //
    //    - `m = (2 ^ i) * c` where i is an integer, then
    //    - `gcd(m, n) = gcd(c, n)`.
    //
    // 5. `gcd(m, n) = gcd(|m - n|, min(m, n))`, where `m = 2 * a + 1` and `n = 2 * b + 1`:
    //
    //    If both `m` and `n` are odd, apply a step of the Euclidean algorithm.
    fn gcd(&self, other: &Self) -> Self;
}

#[cfg(test)]
macro_rules! impl_integer_tests {
    ($type:ty) => {
        mod adjacent {
            use super::*;

            fn get_adjacent<N: Integer>(n: N) -> Vec<N> {
                Adjacent::from(n).collect()
            }

            fn check<N>()
            where
                N: Integer + TryFrom<u8>,
                N::Error: std::fmt::Debug,
            {
                assert_eq!(get_adjacent(N::MIN), [N::MIN + N::ONE]);
                assert_eq!(get_adjacent(N::MAX), [N::MAX - N::ONE]);

                let two = N::try_from(2).unwrap();

                assert_eq!(get_adjacent(N::MIN + N::ONE), [N::MIN, N::MIN + two]);
                assert_eq!(get_adjacent(N::MAX - N::ONE), [N::MAX - two, N::MAX]);
            }

            #[test]
            fn adjacent() {
                check::<$type>();

                let max = i8::MAX as $type / 2;

                (1..max).for_each(|n| {
                    assert_eq!(n.adjacent().sum::<$type>(), 2 * n);
                });
            }
        }
    };
}

macro_rules! impl_unsigned_integer {
    ($type:ident) => {
        impl Integer for $type {
            fn gcd(&self, &(mut n): &Self) -> Self {
                let &(mut m) = self;

                // From identity 1, the order of m and n does not matter: they may be swapped or
                // matched to any of the identities regardless of their order.
                match (m, n) {
                    // Identity 2.
                    (0, p) | (p, 0) => p,

                    _ => {
                        // The number of common factors of 2, described in identity 3.
                        //
                        // Example:
                        //
                        // | Number | Decimal | Decomposition | k  | 16  | 8   | 4   | 2   | 1   |
                        // | ------ | ------- | ------------  | -- | --- | --- | --- | --- | --- |
                        // | m      | 20      | (2 ^ 2) * 5   | 2  | 1   | 0   | 1   | 0   | 0   |
                        // | n      | 24      | (2 ^ 3) * 3   | 3  | 1   | 1   | 0   | 0   | 0   |
                        // | m OR n | 28      | (2 ^ 2) * 7   | 2  | 1   | 1   | 1   | 0   | 0   |
                        let k = (m | n).trailing_zeros();

                        // From identity 3: having computed k, substitute m and n for a and b.
                        // This ensures m and n are odd.
                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        // Apply identity 5 until m == n.
                        while m != n {
                            // Ensure min(m, n) = m.
                            if m > n {
                                mem::swap(&mut m, &mut n);
                            }

                            // Update n to be |m - n|.
                            // This makes n even while m is odd; thus, apply identity 4.
                            // Applying identity 4 ensures n is odd again.
                            n -= m;
                            n >>= n.trailing_zeros();
                        }
                        // Restore the common factors of 2.
                        m << k
                    }
                }
            }
        }

        #[cfg(test)]
        mod $type {
            use super::*;

            impl_integer_tests!($type);

            #[test]
            fn unsigned_gcd() {
                const X: $type = 2 * 3 * 5 * 7;

                assert_eq!(X.gcd(&X), X);
                assert_eq!(X.gcd(&0), X);
                assert_eq!(X.gcd(&(2 * 5 * 7)), 2 * 5 * 7);
                assert_eq!(X.gcd(&(3 * 5 * 7)), 3 * 5 * 7);
                assert_eq!(X.gcd(&(2 * 7 * 11)), 2 * 7);
                assert_eq!(X.gcd(&(2 * 7 * 11)), 2 * 7);
                assert_eq!((30 as $type).gcd(&3), 3);
                assert_eq!((10 as $type).gcd(&2), 2);
            }
        }
    };
}

macro_rules! impl_signed_integer {
    ($type:ident) => {
        impl Integer for $type {
            // This is similar to the unsigned algorithm with a few exceptions:
            // 1. abs() is called on m and n as needed to ensure that the GCD is positive.
            // 2. Patterns are added to account for the minimum values for a given type.
            // 3. Given that for any signed type, Self::MIN.abs() cannot be represented as a Self:
            //    attempting to calculate a GCD that should return Self::MIN.abs() will cause an
            //    overflow. Code in debug mode will trigger a panic in this case, while optimised
            //    code will return Self::MIN (a negative value) without a panic.
            fn gcd(&self, &(mut n): &Self) -> Self {
                let &(mut m) = self;
                match (m, n) {
                    (0, p) | (p, 0) => p.abs(),

                    // Self::MIN is guaranteed to be a power of 2; thus, the GCD of Self::MIN and
                    // any other non-zero Self, p, is the greatest power of 2 that divides p.
                    (Self::MIN, p) | (p, Self::MIN) => ((1 as Self) << p.trailing_zeros()).abs(),

                    _ => {
                        let k = (m | n).trailing_zeros();

                        m = m.abs();
                        n = n.abs();

                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        while m != n {
                            if m > n {
                                mem::swap(&mut m, &mut n);
                            }
                            n -= m;
                            n >>= n.trailing_zeros();
                        }
                        m << k
                    }
                }
            }
        }

        #[cfg(test)]
        mod $type {
            use super::*;

            impl_integer_tests!($type);

            #[test]
            fn signed_gcd() {
                const X: $type = 2 * 3 * 5;

                assert_eq!(X.gcd(&X), X);
                assert_eq!(X.gcd(&0), X);
                assert_eq!(X.gcd(&(2 * 5)), 2 * 5);
                assert_eq!((-X).gcd(&(3 * 5)), 3 * 5);
                assert_eq!(X.gcd(&(2 * 11)), 2);
                assert_eq!((-X).gcd(&(2 * 11)), 2);
                assert_eq!((30 as $type).gcd(&3), 3);
                assert_eq!(((-10) as $type).gcd(&2), 2);
            }

            #[test]
            fn signed_min_gcd() {
                assert_eq!(<$type>::MIN.gcd(&4), 4);
                assert_eq!(<$type>::MIN.gcd(&2), 2);
                assert_eq!(<$type>::MIN.gcd(&3), 1);
                assert_eq!(<$type>::MIN.gcd(&1), 1);
            }

            #[cfg(debug_assertions)]
            mod debug_tests {
                use super::*;

                #[test]
                #[should_panic(expected = "attempt to negate with overflow")]
                fn signed_gcd_overflow_with_zero() {
                    <$type>::MIN.gcd(&0);
                }

                #[test]
                #[should_panic(expected = "attempt to negate with overflow")]
                fn signed_gcd_overflow_with_min() {
                    <$type>::MIN.gcd(&<$type>::MIN);
                }
            }
        }
    };
}

macro_rules! repeat_macro_for_unsigned_integral {
    ($macro:ident) => {
        $crate::repeat_macro! {
            $macro for
                u8 u16 u32 u64 u128 usize
        }
    };
}

macro_rules! repeat_macro_for_signed_integral {
    ($macro:ident) => {
        $crate::repeat_macro! {
            $macro for
                i8 i16 i32 i64 i128 isize
        }
    };
}

macro_rules! repeat_macro_for_integral {
    ($macro:ident) => {
        $crate::integer::repeat_macro_for_unsigned_integral!($macro);
        $crate::integer::repeat_macro_for_signed_integral!($macro);
    };
}

pub(crate) use repeat_macro_for_integral;
pub(crate) use repeat_macro_for_signed_integral;
pub(crate) use repeat_macro_for_unsigned_integral;

repeat_macro_for_unsigned_integral!(impl_unsigned_integer);
repeat_macro_for_signed_integral!(impl_signed_integer);
