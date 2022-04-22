#![warn(missing_docs)]

//! Basic library function for [AoC 2021](https://adventofcode.com/2021/).

use std::fmt::Display;
use std::{env, fs};

/// A module providing an interface for bit operations.
pub mod bits;

/// A module providing an interface for dealing with errors.
pub mod errors;

/// A module providing an interface for integral types.
pub mod integer;
use integer::Integer;

/// A module providing iterators.
pub mod iterator;

/// A module that defines a generic, homogeneous pair type.
pub mod pair;

/// A module providing an interface for searching over types.
pub mod search;

/// A module providing an extension trait to the `str` type.
pub mod str;

/// A module providing an interface for transposing matrix-like types.
pub mod transpose;

/// An interface for AoC puzzles.
pub trait Puzzle {
    /// The type of the solution.
    type Solution: Solution;

    /// Tries to solve this puzzle.
    fn solve(&self, input: String) -> anyhow::Result<Self::Solution>;
}

#[derive(Debug, Default)]
struct IndexedPrinter<N: Integer> {
    index: N,
}

impl<N: Integer> IndexedPrinter<N> {
    fn print<T: Display>(&mut self, t: &T) {
        self.index += N::ONE;
        println!("  Part {}: {}", self.index, t);
    }
}

/// A marker trait for types that customise how they are displayed as a solution.
pub trait SolutionMarker: Display {}

/// An interface for the puzzle solutions.
pub trait Solution {
    /// Prints the solutions.
    fn print(&self);
}

impl<T: Display, const N: usize> Solution for [T; N] {
    fn print(&self) {
        let mut i = IndexedPrinter::<u8>::default();
        self.iter().for_each(|t| i.print(t));
    }
}

impl<T: Display, U: Display> Solution for (T, U) {
    fn print(&self) {
        let mut i = IndexedPrinter::<u8>::default();
        i.print(&self.0);
        i.print(&self.1);
    }
}

impl<S: SolutionMarker> Solution for S {
    fn print(&self) {
        println!("{self}");
    }
}

/// Calls a macro on each token tree.
#[macro_export]
macro_rules! repeat_macro {
    ($macro:ident for $($x:tt)+) => {
        $($macro!($x);)+
    };
}

/// An entry point for the puzzle binaries.
pub fn run(puzzle: impl Puzzle) -> anyhow::Result<()> {
    env::args().skip(1).try_for_each(|arg| {
        let input = fs::read_to_string(&arg)?;
        let solution = puzzle.solve(input)?;
        println!("Input file: {arg}");
        solution.print();
        Ok(())
    })
}

/// Defines `main` for the puzzle binaries.
#[macro_export]
macro_rules! main {
    ($puzzle:ident) => {
        fn main() -> anyhow::Result<()> {
            $crate::run($puzzle)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    struct P;

    impl Puzzle for P {
        type Solution = [usize; 1];
        fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
            Ok([input.len()])
        }
    }

    #[test]
    fn solve() {
        let solutions = P.solve(('0'..='9').collect()).unwrap();
        solutions.print();
        assert_eq!(solutions, [10]);
    }
}
