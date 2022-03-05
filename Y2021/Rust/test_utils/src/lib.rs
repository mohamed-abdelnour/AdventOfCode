#![warn(missing_docs)]

//! A utility crate for testing aoc_2021.

use std::error::Error;
use std::fmt::Debug;
use std::fs;
use std::path::Path;

use aoc_2021::Puzzle;

/// Display the error variant of a result.
pub trait DisplayPanic {
    /// Panic with `std::fmt::Display` formatting.
    fn display_panic(self);
}

impl<T, E: Error> DisplayPanic for Result<T, E> {
    fn display_panic(self) {
        if let Err(e) = self {
            panic!("{}", e);
        }
    }
}

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
