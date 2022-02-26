use std::error::Error;
use std::fmt;
use std::num::ParseIntError;

use aoc_2021::errors::Expectation;

/// The possible parsing errors.
#[derive(Debug)]
pub enum CommandError {
    /// An invalid direction.
    Direction(String),
    /// An invalid command.
    Input(String),
    /// An invalid value.
    Parse(ParseIntError),
}

impl CommandError {
    /// Returns an error given an invalid direction.
    fn invalid_direction(direction: &str) -> Expectation {
        // The valid directions.
        let valid = r#""forward", "down" or "up""#;
        Expectation::new("direction", valid, direction)
    }

    /// Returns an error given an invalid input.
    fn invalid_input(input: &str) -> Expectation {
        Expectation::new("input", "a direction followed by a value", input)
    }
}

impl From<ParseIntError> for CommandError {
    fn from(err: ParseIntError) -> Self {
        Self::Parse(err)
    }
}

impl fmt::Display for CommandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direction(direction) => CommandError::invalid_direction(direction).fmt(f),
            Self::Input(input) => CommandError::invalid_input(input).fmt(f),
            Self::Parse(err) => err.fmt(f),
        }
    }
}

impl Error for CommandError {}
