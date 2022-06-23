#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 02.](https://adventofcode.com/2021/day/2)

use std::str::FromStr;

use crate::{pair::Pair, Puzzle};

/// A module for errors that can arise on parsing a command.
mod errors;
use errors::CommandError;

/// The command after parsing.
#[derive(Debug, Eq, PartialEq)]
enum Command {
    /// "forward" X.
    Horizontal(i32),
    /// "down" X (denoted by a positive value) or "up" X (denoted by a negative value).
    Vertical(i32),
}

impl FromStr for Command {
    type Err = CommandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Split by a single space.
        let command = s.split(' ').collect::<Vec<_>>();
        // Match to an array of size 2.
        if let [direction, value] = command[..] {
            // Try to parse the value first.
            let value = value.parse()?;

            // Try to parse the direction.
            match direction {
                "forward" => Ok(Command::Horizontal(value)),
                "down" => Ok(Command::Vertical(value)),
                "up" => Ok(Command::Vertical(-value)),
                _ => Err(CommandError::Direction(direction.into())),
            }
        } else {
            // If the 2-array pattern fails, generate an error from the entire command.
            Err(CommandError::Input(s.into()))
        }
    }
}

/// The position as defined in part 1.
#[derive(Debug, Default)]
struct Position {
    /// The horizontal position.
    horizontal: i32,
    /// The depth.
    depth: i32,
}

impl From<Position> for Pair<i32> {
    fn from(Position { horizontal, depth }: Position) -> Self {
        Pair(horizontal, depth)
    }
}

/// The position as defined in part 2.
#[derive(Debug, Default)]
struct AimedPosition {
    /// A wrapper around `Position`.
    position: Position,
    /// The aim.
    aim: i32,
}

/// This puzzle.
pub struct D02;

impl Puzzle for D02 {
    type Solution = [i32; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut position = Position::default();
        let AimedPosition {
            position: mut aimed,
            mut aim,
        } = AimedPosition::default();

        let step = |command: Result<_, _>| {
            command.map(|command| match command {
                // "forward" `v`
                Command::Horizontal(v) => {
                    // Part 1: add `v` to the horizontal position.
                    position.horizontal += v;

                    // Part 2: add `v` to the horizontal position and (aim * `v`) to the depth.
                    aimed.horizontal += v;
                    aimed.depth += aim * v;
                }
                // "down" `v` or "up" `-v`
                Command::Vertical(v) => {
                    // Part 1: add `v` to the depth.
                    position.depth += v;

                    // Part 2: add `v` to the aim.
                    aim += v;
                }
            })
        };

        input.lines().map(str::parse).try_for_each(step)?;

        Ok([
            Pair::from(position).into_iter().product(),
            Pair::from(aimed).into_iter().product(),
        ])
    }
}
