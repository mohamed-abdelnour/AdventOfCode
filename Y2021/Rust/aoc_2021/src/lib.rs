#![warn(missing_docs)]

//! Basic library function for [AoC 2021](https://adventofcode.com/2021/).

use std::{
    env,
    fmt::{Debug, Display},
    fs,
    path::Path,
};

pub mod bits;
pub mod errors;
pub mod extension_traits;
pub mod integer;
pub mod iterator;
pub mod min_heap;
pub mod pair;
pub mod puzzles;
pub mod search;
pub mod transpose;

use integer::Integer;

/// Alias for `anyhow::Error`;
pub type Error = anyhow::Error;

/// Alias for `anyhow::Result<T>`;
pub type Result<T> = anyhow::Result<T>;

/// An interface for AoC puzzles.
pub trait Puzzle: Sized {
    /// The type of the solution.
    type Solution: Solution;

    /// Tries to solve this puzzle given the puzzle input, `input`.
    fn solve(&self, input: String) -> anyhow::Result<Self::Solution>;

    /// Reads the puzzle input from the file at `path` and tries to solve the puzzle.
    fn solve_file(&self, path: impl AsRef<Path>) -> anyhow::Result<Self::Solution> {
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
    ($module:ident::$puzzle:ident) => {
        use $crate::{puzzles::$module::$puzzle, Puzzle};

        fn main() -> $crate::Result<()> {
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
