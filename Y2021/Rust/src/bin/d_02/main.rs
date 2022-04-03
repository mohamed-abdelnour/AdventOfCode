#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 02.](https://adventofcode.com/2021/day/2)

use std::str::FromStr;

use aoc_2021::pair::Pair;
use aoc_2021::Puzzle;

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
struct D02;

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

aoc_2021::main!(D02);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D02.check("../Inputs/D02/sample.txt", [150, 900]);
    }

    #[test]
    fn input() {
        D02.check("../Inputs/D02/input.txt", [1_714_680, 1_963_088_820]);
    }
}

#[cfg(test)]
mod private {
    use test_utils::*;

    use super::*;

    fn parser(s: &str) -> Command {
        s.parse().unwrap()
    }

    #[test]
    fn parse_success() {
        assert_eq!(parser("forward 5"), Command::Horizontal(5));
        assert_eq!(parser("down 8"), Command::Vertical(8));
        assert_eq!(parser("up 3"), Command::Vertical(-3));
    }

    #[test]
    #[should_panic(expected = "InvalidDigit")]
    fn parse_fail_value() {
        parser("forward x");
    }

    fn panic_parser(s: &str) {
        s.parse::<Command>().display_panic();
    }

    #[test]
    #[should_panic(
        expected = r#"invalid direction: expected "forward", "down" or "up", got "backward""#
    )]
    fn parse_fail_direction() {
        panic_parser("backward 10");
    }

    #[test]
    #[should_panic(
        expected = r#"invalid input: expected a direction followed by a value, got "up down 10""#
    )]
    fn parse_fail_input() {
        panic_parser("up down 10");
    }
}
