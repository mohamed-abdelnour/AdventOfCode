use std::{fmt::Debug, path::Path};

use aoc_2021::Puzzle;
use utils::input_dir;

pub trait PuzzleExt: Sized + Puzzle {
    fn check(self, path: impl AsRef<Path>, expected: Self::Solution)
    where
        Self::Solution: Debug + PartialEq<Self::Solution>,
    {
        let file = input_dir(path).unwrap();
        assert_eq!(self.solve_file(file).unwrap(), expected);
    }
}

impl<P: Puzzle> PuzzleExt for P {}
