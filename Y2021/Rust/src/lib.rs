#![warn(missing_docs)]

//! Basic library function for [AoC 2021](https://adventofcode.com/2021/).

use std::fmt::Display;
use std::{env, fs};

use anyhow::Result;

/// A module providing an interface for binary operations on numbers.
pub mod binary;
/// A module providing an interface for dealing with errors.
pub mod errors;
/// A module providing an interface for numeric types.
pub mod num;
/// A module providing trait extensions.
pub mod trait_exts;

/// An interface for for AoC puzzles.
pub trait Puzzle {
    /// The type of the solution.
    type Solution: Solution;

    /// Tries to solve this puzzle.
    fn solve(&self, input: String) -> Result<Self::Solution>;
}

/// An interface for the puzzle solutions.
pub trait Solution {
    /// Prints the solutions.
    fn print(&self);
}

impl<T: Display, const N: usize> Solution for [T; N] {
    fn print(&self) {
        self.iter()
            .enumerate()
            .map(|(i, v)| (i + 1, v))
            .for_each(|(i, v)| println!("  Part {i}: {v}"));
    }
}

/// A marker trait for puzzle solutions that shall be directly displayed.
pub trait SimpleSolution: Display {}

impl<S: SimpleSolution> Solution for S {
    fn print(&self) {
        println!("Solution: {self}");
    }
}

macro_rules! impl_simple_solution {
    ($type:ty) => {
        impl SimpleSolution for $type {}
    };
}

macro_rules! repeat_macro {
    ($macro:ident for $($x:tt)+) => {
        $($macro!($x);)+
    };
}

pub(crate) use repeat_macro;

repeat_macro!(impl_simple_solution for usize);

/// An entry point for the puzzle binaries.
pub fn run(puzzle: impl Puzzle) -> Result<()> {
    env::args().skip(1).try_for_each(|arg| -> Result<()> {
        println!("Input file: {arg}");
        let input = fs::read_to_string(arg)?;

        puzzle.solve(input)?.print();

        Ok(())
    })
}

/// Defines `main` for the puzzle binaries.
#[macro_export]
macro_rules! main {
    ($puzzle:ident) => {
        fn main() -> anyhow::Result<()> {
            aoc_2021::run($puzzle)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    struct P;

    impl Puzzle for P {
        type Solution = usize;
        fn solve(&self, input: String) -> Result<Self::Solution> {
            Ok(input.len())
        }
    }

    #[test]
    fn solve() {
        let solutions = P.solve(('0'..='9').collect()).unwrap();
        solutions.print();
        assert_eq!(solutions, 10);
    }
}
