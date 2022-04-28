use std::fmt::Debug;
use std::path::Path;

use aoc_2021::Puzzle;

pub trait PuzzleExt: Sized + Puzzle {
    fn check(self, path: impl AsRef<Path>, expected: Self::Solution)
    where
        Self::Solution: Debug + PartialEq<Self::Solution>,
    {
        assert_eq!(self.solve_file(path).unwrap(), expected);
    }
}

impl<P: Puzzle> PuzzleExt for P {}
