#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 04.](https://adventofcode.com/2021/day/4)

use std::collections::HashMap;
use std::num::ParseIntError;
use std::ops::ControlFlow;

use aoc_2021::define_error;
use aoc_2021::pair::Pair;
use aoc_2021::Puzzle;

define_error!(BingoFormatError, "did not find a blank line");

/// The side length of a bingo board.
const SIZE: usize = 5;

/// The position of a cell on a bingo board.
type Position = Pair<usize>;

/// An interface for keeping track of the state of the game.
trait State {
    /// The index of the element to update.
    type Index;

    /// Update the state at `index` and check if the board wins.
    fn update_check(&mut self, index: Self::Index) -> bool;
}

impl State for [usize; SIZE] {
    type Index = usize;

    fn update_check(&mut self, index: Self::Index) -> bool {
        self[index] += 1;
        self[index] == SIZE
    }
}

/// A bingo board.
#[derive(Debug, Default)]
struct Board {
    /// The cells in a board.
    cells: HashMap<u32, Position>,
    /// The count of marked numbers per row.
    rows: [usize; SIZE],
    /// The count of marked numbers per column.
    columns: [usize; SIZE],
    /// Whether or not the board has won.
    is_done: bool,
}

impl Board {
    /// Marks a number on the board (if it exists), removing it from the board and checking if the
    /// board has won.
    fn mark(&mut self, number: u32) {
        if let Some(&p) = self.cells.get(&number) {
            self.cells.remove(&number);
            self.is_done = self.update_check(p);
        }
    }
}

impl State for Board {
    type Index = Position;

    fn update_check(&mut self, Pair(x, y): Self::Index) -> bool {
        self.rows.update_check(x) || self.columns.update_check(y)
    }
}

impl TryFrom<&str> for Board {
    type Error = ParseIntError;

    // For each position in `(0..SIZE).flat_map(|row| std::iter::repeat(row).zip(0..SIZE))`, finds
    // the number at this position, constructs the tuple (number, position) and collects the pairs
    // into a `Board`.
    fn try_from(board: &str) -> Result<Self, Self::Error> {
        board
            .lines()
            .enumerate()
            .flat_map(|(x, row)| {
                row.split_ascii_whitespace()
                    .enumerate()
                    .map(move |(y, column)| Ok((column.parse()?, Pair(x, y))))
            })
            .collect()
    }
}

impl FromIterator<(u32, Position)> for Board {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (u32, Position)>,
    {
        Self {
            cells: iter.into_iter().collect(),
            ..Default::default()
        }
    }
}

/// All the bingo boards in the game.
#[derive(Debug)]
struct Game(Vec<Board>);

impl Game {
    /// Plays the game until all boards win.
    fn play(&mut self, order: Vec<u32>) -> [u32; 2] {
        let Self(boards) = self;
        let original_length = boards.len();
        let mut solution = <[u32; 2]>::default();

        // Marks a number off all boards that are yet to win.
        let mark = |number| {
            let mut i = 0;
            while let Some(board) = boards.get_mut(i) {
                board.mark(number);

                if board.is_done {
                    let current_length = boards.len();
                    let board = boards.swap_remove(i);

                    // Calculate the sum of the remaining (unmarked) numbers, multiply it by the
                    // the number that has just been marked and put the result at `index` in the
                    // solution array.
                    let mut part = |index| {
                        let sum: u32 = board.cells.keys().sum();
                        solution[index] = number * sum;
                    };

                    // Call `part` only for the first and last boards to win.
                    if current_length == original_length {
                        part(0);
                    } else if current_length == 1 {
                        part(1);
                    }
                } else {
                    // If `swap_remove` was called, check the current index again; otherwise,
                    // increment the index.
                    i += 1;
                }
            }

            // If all the boards have won, do not keep iterating over the numbers in the order.
            if boards.is_empty() {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        };

        order.into_iter().try_for_each(mark);
        solution
    }
}

impl FromIterator<Board> for Game {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Board>,
    {
        Self(iter.into_iter().collect())
    }
}

impl TryFrom<&str> for Game {
    type Error = ParseIntError;

    fn try_from(boards: &str) -> Result<Self, Self::Error> {
        boards.split("\n\n").map(Board::try_from).collect()
    }
}

/// This puzzle.
struct D04;

impl Puzzle for D04 {
    type Solution = [u32; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let (order, boards) = input.split_once("\n\n").ok_or(BingoFormatError)?;
        let order: Vec<_> = order.split(',').map(str::parse).collect::<Result<_, _>>()?;
        Ok(Game::try_from(boards)?.play(order))
    }
}

aoc_2021::main!(D04);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D04.check("../Inputs/D04/sample.txt", [4512, 1924]);
    }

    #[test]
    fn input() {
        D04.check("../Inputs/D04/input.txt", [54_275, 13_158]);
    }
}
