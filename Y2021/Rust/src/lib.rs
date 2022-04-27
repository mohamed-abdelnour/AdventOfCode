#![warn(missing_docs)]

//! Basic library function for [AoC 2021](https://adventofcode.com/2021/).

use std::fmt::{Debug, Display};
use std::path::Path;
use std::{env, fs};

/// A module providing an interface for bit operations.
pub mod bits;

/// A module providing an interface for dealing with errors.
pub mod errors;

/// A module providing extension traits.
pub mod extension_traits;

/// A module providing an interface for integral types.
pub mod integer;
use integer::Integer;

/// A module providing iterators.
pub mod iterator;

/// A module that defines a generic, homogeneous pair type.
pub mod pair;

/// A module providing an interface for searching over types.
pub mod search;

/// A module providing an interface for transposing matrix-like types.
pub mod transpose;

/// An interface for AoC puzzles.
pub trait Puzzle: Sized {
    /// The type of the solution.
    type Solution: Solution;

    /// Tries to solve this puzzle given the puzzle input, `input`.
    fn solve(&self, input: String) -> anyhow::Result<Self::Solution>;

    /// Reads the puzzle input from the file at `path` and tries to solve the puzzle.
    fn solve_file(&self, path: impl AsRef<Path> + Display) -> anyhow::Result<Self::Solution> {
        let input = fs::read_to_string(&path)?;
        self.solve(input)
    }

    /// Calls `self.solve_file` with `path` and outputs the solution if it succeeds.
    fn run_file(&self, path: impl AsRef<Path> + Display) -> anyhow::Result<()> {
        self.solve_file(&path).map(|solution| {
            println!("Input file: {path}");
            solution.print();
        })
    }

    /// The main entry point for the puzzle binaries: reads file paths from the command line calls
    /// `self.run_file` for each one.
    fn run(&self) -> anyhow::Result<()> {
        env::args()
            .skip(1)
            .try_for_each(|arg| self.run_file(arg).map(drop))
    }
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

/// Defines `main` for the puzzle binaries.
#[macro_export]
macro_rules! main {
    ($puzzle:ident) => {
        fn main() -> anyhow::Result<()> {
            $puzzle.run()
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
