#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 13.](https://adventofcode.com/2021/day/13)

use std::collections::HashSet;
use std::str::{FromStr, Lines};
use std::{fmt, mem};

use aoc_2021::extension_traits::{str::StrExt, vec::VecExt};
use aoc_2021::{define_error, Puzzle, SolutionMarker};

/// A dot on the paper.
const DOT: char = '\u{2588}';

/// An empty, unmarked position.
const UNMARKED: char = ' ';

define_error!(
    ParseInstructionError,
    r#"an instruction must match ".*[xy]=[0-9]*""#
);

define_error!(
    EmptyInstructionsError,
    "a sheet must have at least one fold instruction"
);

define_error!(
    ParseSheetError,
    "a sheet must consist of dots and instructions separated by a blank line"
);

define_error!(ParseDotError, r#"a dot must match "[0-9]*,[0-9]*""#);

/// The position of a dot on the paper.
#[derive(Debug, Default, Eq, Hash, PartialEq)]
struct Coordinate {
    /// The horizontal position, `x`.
    col: u16,

    /// The vertical position, `y`.
    row: u16,
}

/// A fold instruction.
#[derive(Debug)]
enum Instruction {
    /// `x=...`
    Left(u16),

    /// `y=...`
    Up(u16),
}

impl Instruction {
    /// Applies a fold instruction to a coordinate.
    fn fold(&self, mut p: Coordinate) -> Coordinate {
        // - For all p: {p <= axis}, p does not move.
        // - For all p: {p > axis}, p moves to axis - (p - axis) => 2 * axis - p.
        //
        // Note: this does not account for 2 * axis - p < 0, i.e., when folding would move a point
        // before 0. It is not explicitly stated in the puzzle that this cannot happen, but it does
        // not happen neither with the sample nor with my puzzle input, which is why it is not
        // accounted for. An input that causes this to happen would trigger a panic while running
        // in debug mode, so it would be easy to catch.
        let fold = |p, axis| if p <= axis { p } else { 2 * axis - p };

        match *self {
            Self::Left(col) => p.col = fold(p.col, col),
            Self::Up(row) => p.row = fold(p.row, row),
        }

        p
    }
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(instruction: &str) -> Result<Self, Self::Err> {
        let (text, num) = instruction.split_once('=').ok_or(ParseInstructionError)?;

        let num = num.parse::<u16>()?;

        text.chars()
            .next_back()
            .ok_or(ParseInstructionError)
            .and_then(|direction| match direction {
                'x' => Ok(Self::Left(num)),
                'y' => Ok(Self::Up(num)),
                _ => Err(ParseInstructionError),
            })
            .map_err(Into::into)
    }
}

/// The possible states for a position on the sheet.
#[derive(Clone, Debug, Eq, PartialEq)]
enum Position {
    /// A position marked with a dot.
    Dot,

    /// An empty, unmarked position.
    Unmarked,
}

impl Default for Position {
    fn default() -> Self {
        Self::Unmarked
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Self::Dot => DOT,
            Self::Unmarked => UNMARKED,
        };
        write!(f, "{c}")
    }
}

/// The transparent sheet.
#[derive(Debug)]
struct Sheet<'l> {
    /// The positions of the dots currently visible on the sheet.
    dots: HashSet<Coordinate>,

    /// An iterator over the fold instructions.
    instructions: Lines<'l>,
}

impl Iterator for Sheet<'_> {
    type Item = Result<usize, anyhow::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.instructions.next().map(|instruction| {
            let instruction = instruction.parse::<Instruction>()?;

            self.dots = mem::take(&mut self.dots)
                .into_iter()
                .map(|dot| instruction.fold(dot))
                .collect();

            Ok(self.dots.len())
        })
    }
}

impl<'l> TryFrom<&'l str> for Sheet<'l> {
    type Error = anyhow::Error;

    fn try_from(sheet: &'l str) -> Result<Self, Self::Error> {
        let (dots, instructions) = sheet
            .split_once(sheet.double_line_ending())
            .ok_or(ParseSheetError)?;

        let dots = dots
            .lines()
            .map(|dot| {
                dot.split_once(',')
                    .ok_or(ParseDotError)
                    .map_err(anyhow::Error::from)
                    .and_then(|(x, y)| {
                        let col = x.parse()?;
                        let row = y.parse()?;

                        Ok(Coordinate { col, row })
                    })
                    .map_err(anyhow::Error::from)
            })
            .collect::<Result<_, _>>()?;

        let instructions = instructions.lines();

        Ok(Self { dots, instructions })
    }
}

impl From<Sheet<'_>> for Vec<Vec<Position>> {
    fn from(sheet: Sheet) -> Self {
        let mut matrix: Vec<Vec<Position>> = Vec::new();

        sheet.dots.into_iter().for_each(|dot| {
            let row = usize::from(dot.row);
            let col = usize::from(dot.col);

            matrix.ensure_index(row);
            matrix[row].ensure_index(col);

            matrix[row][col] = Position::Dot;
        });

        matrix
    }
}

/// A type that customises how the puzzle solution is displayed.
#[derive(Debug, Eq, PartialEq)]
struct Solution {
    /// Part 1.
    overlaps: usize,

    /// Part 2.
    code: Vec<Vec<Position>>,
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "  Part 1: {}", self.overlaps)?;

        write!(f, "  Part 2:")?;
        self.code.iter().try_for_each(|row| {
            writeln!(f)?;
            write!(f, "    ")?;
            row.iter().try_for_each(|column| write!(f, "{column}"))
        })
    }
}

impl SolutionMarker for Solution {}

/// This puzzle.
struct D13;

impl Puzzle for D13 {
    type Solution = Solution;

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut sheet = Sheet::try_from(&*input)?;

        let overlaps = sheet.next().ok_or(EmptyInstructionsError)??;

        sheet.try_for_each(|l| l.map(drop))?;
        let code = Vec::from(sheet);

        Ok(Solution { overlaps, code })
    }
}

aoc_2021::main!(D13);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    impl From<char> for Position {
        fn from(c: char) -> Self {
            match c {
                DOT => Self::Dot,
                UNMARKED => Self::Unmarked,
                _ => unreachable!(),
            }
        }
    }

    fn grid(s: &str) -> Vec<Vec<Position>> {
        s.lines()
            .skip(1)
            .map(|l| l.chars().map(Into::into).collect())
            .collect()
    }

    #[test]
    fn sample() {
        const CODE: &str = "
█████
█   █
█   █
█   █
█████";

        let overlaps = 17;
        let code = grid(CODE);
        D13.check("../Inputs/D13/sample.txt", Solution { overlaps, code });
    }

    #[test]
    fn input() {
        const CODE: &str = "
 ██  ███  ████ █    ███  ████ ████ █
█  █ █  █    █ █    █  █ █       █ █
█    █  █   █  █    █  █ ███    █  █
█    ███   █   █    ███  █     █   █
█  █ █    █    █    █    █    █    █
 ██  █    ████ ████ █    █    ████ ████";

        let overlaps = 607;
        let code = grid(CODE);
        D13.check("../Inputs/D13/input.txt", Solution { overlaps, code });
    }
}
