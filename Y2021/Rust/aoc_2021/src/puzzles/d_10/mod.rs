#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 10.](https://adventofcode.com/2021/day/10)

use std::{
    ops::{ControlFlow, Not},
    str::FromStr,
};

use crate::{define_error, repeat_macro, Puzzle};

define_error!(
    IllegalCharacterError,
    "the allowed characters are '(', ')', '[', ']', '{', '}', '<' and '>'"
);

/// The factor to multiply the total score by for each character in part 2.
const COMPLETION_SCORE_BASE: u64 = 5;

/// The legal character types.
#[derive(Debug, Eq, PartialEq)]
enum CharType {
    /// '(' or ')'
    Parenthesis,
    /// '[' or ']'
    Bracket,
    /// '{' or '}'
    Brace,
    /// '<' or '>'
    Chevron,
}

/// The legal characters.
#[derive(Debug, Eq, PartialEq)]
enum Char {
    /// The left variants for each `CharType`.
    Left(CharType),
    /// The right variants for each `CharType`.
    Right(CharType),
}

impl Not for Char {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Left(c) => Self::Right(c),
            Self::Right(c) => Self::Left(c),
        }
    }
}

impl TryFrom<u8> for Char {
    type Error = IllegalCharacterError;

    fn try_from(b: u8) -> Result<Self, Self::Error> {
        match b {
            b'(' => Ok(Self::Left(CharType::Parenthesis)),
            b'[' => Ok(Self::Left(CharType::Bracket)),
            b'{' => Ok(Self::Left(CharType::Brace)),
            b'<' => Ok(Self::Left(CharType::Chevron)),

            b')' => Ok(Self::Right(CharType::Parenthesis)),
            b']' => Ok(Self::Right(CharType::Bracket)),
            b'}' => Ok(Self::Right(CharType::Brace)),
            b'>' => Ok(Self::Right(CharType::Chevron)),

            _ => Err(IllegalCharacterError),
        }
    }
}

/// Implements `From<CharType>` and `From<Char>` for a type given the scores for each character.
macro_rules! define_scores {
    (($scores:ty, $p:literal, $bs:literal, $bc:literal, $c:literal)) => {
        impl From<CharType> for $scores {
            fn from(char_type: CharType) -> Self {
                let score = match char_type {
                    CharType::Parenthesis => $p,
                    CharType::Bracket => $bs,
                    CharType::Brace => $bc,
                    CharType::Chevron => $c,
                };
                Self(score)
            }
        }

        impl From<Char> for $scores {
            fn from(c: Char) -> Self {
                match c {
                    Char::Left(char_type) | Char::Right(char_type) => char_type.into(),
                }
            }
        }
    };
}

/// The scoring as described in part 1.
struct EntryScore(u64);

/// The scoring as described in part 2.
struct CompletionScore(u64);

repeat_macro! {
    define_scores for
        (EntryScore, 3, 57, 1197, 25_137)
        (CompletionScore, 1, 2, 3, 4)
}

/// Whether an entry is corrupted or just incomplete.
#[derive(Debug)]
enum EntryState {
    /// The entry is incomplete.
    Incomplete,
    /// The entry is corrupted; this also holds the score for a corrupted entry.
    Corrupted(u64),
}

impl Default for EntryState {
    fn default() -> Self {
        Self::Incomplete
    }
}

/// An entry in the navigation subsystem.
#[derive(Debug, Default)]
struct Entry {
    /// A stack that holds the left variants of the characters.
    stack: Vec<Char>,
    /// The state of this entry.
    state: EntryState,
}

impl Entry {
    /// Checks if parsing `current` corrupts this entry and computes the entry's score in that
    /// case.
    fn corrupts(&mut self, current: Char) {
        if self.stack.pop().map_or(true, |delim| !delim != current) {
            let EntryScore(score) = current.into();
            self.state = EntryState::Corrupted(score);
        }
    }

    /// Returns the score for the completion string.
    fn completion_score(self) -> u64 {
        self.stack
            .into_iter()
            .map(CompletionScore::from)
            .rfold(0, |acc, CompletionScore(score)| {
                COMPLETION_SCORE_BASE * acc + score
            })
    }
}

/// The solution to both parts of the puzzle.
struct Scores {
    /// Part 1.
    entry: u64,
    /// Part 2.
    completion: u64,
}

impl FromStr for Scores {
    type Err = IllegalCharacterError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut completion_scores = Vec::new();

        let entry = s.lines().try_fold(0, |mut acc, line| {
            let mut result = Ok(());
            let mut entry = Entry::default();

            line.bytes().try_for_each(|c| {
                c.try_into().map_or_else(
                    // Capture the error and short-circuit.
                    |err| {
                        result = Err(err);
                        ControlFlow::Break(())
                    },
                    |c| {
                        // Push opening delimiters on to the stack. For closing delimiters, check
                        // if there is a corresponding opening delimiter; otherwise, the entry is
                        // corrupted.
                        match c {
                            Char::Left(_) => entry.stack.push(c),
                            Char::Right(_) => entry.corrupts(c),
                        }

                        // If the entry is corrupted, update the score and short-circuit.
                        match entry.state {
                            EntryState::Incomplete => ControlFlow::Continue(()),
                            EntryState::Corrupted(score) => {
                                acc += score;
                                ControlFlow::Break(())
                            }
                        }
                    },
                )
            });

            // Short-circuit if an error occurred.
            result?;

            if let EntryState::Incomplete = entry.state {
                completion_scores.push(entry.completion_score());
            }

            Ok(acc)
        })?;

        completion_scores.sort_unstable();
        let completion = completion_scores[completion_scores.len() / 2];

        Ok(Self { entry, completion })
    }
}

/// This puzzle.
pub struct D10;

impl Puzzle for D10 {
    type Solution = [u64; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let Scores { entry, completion } = input.parse()?;
        Ok([entry, completion])
    }
}
