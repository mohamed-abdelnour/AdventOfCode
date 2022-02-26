#![warn(missing_docs)]

//! A utility crate for testing aoc_2021.

use std::fmt::Debug;
use std::fs;
use std::path::Path;

use aoc_2021::Puzzle;

/// An extension trait to `aoc_2021::Puzzle`.
pub trait PuzzleExt: Sized + Puzzle {
    /// Checks that the solution for this puzzle is correct.
    fn check(self, path: impl AsRef<Path>, expected: Self::Solution)
    where
        Self::Solution: Debug + PartialEq<Self::Solution>,
    {
        let input = fs::read_to_string(path).unwrap();
        assert_eq!(self.solve(input).unwrap(), expected);
    }
}

impl<P: Puzzle> PuzzleExt for P {}
