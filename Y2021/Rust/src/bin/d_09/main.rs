#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 09.](https://adventofcode.com/2021/day/9)

use std::collections::BinaryHeap;
use std::str::FromStr;
use std::{iter, mem};

use aoc_2021::errors::{EmptyInputError, ParseDigitError};
use aoc_2021::extension_traits::{binary_heap::BinaryHeapExt, u8::U8Ext};
use aoc_2021::pair::{greater::Greater, Pair};
use aoc_2021::{define_error, Puzzle};

define_error!(NotEnoughBasinsError, "less then 3 basins were found");

/// The highest a location can be.
const MAX_HEIGHT: usize = 9;

/// The number of basins needed for the solution of part 2.
const P2_BASINS: usize = 3;

/// A position in the grid.
type Point = Pair<usize>;

/// The height of a particular location. This is set to `None` whenever the cell is visited while
/// counting the sizes of the basins (to avoid counting a location more than once).
type Cell = Option<usize>;

/// A wrapper around a row in the grid that implements `std::str::FromStr`.
#[derive(Debug)]
struct Row(Vec<Cell>);

impl FromStr for Row {
    type Err = ParseDigitError;

    fn from_str(l: &str) -> Result<Self, Self::Err> {
        // CAST: a u8 is guaranteed to fit in a usize.
        let parse_byte = |b: u8| b.parse_digit().map(|d| Some(d as usize));

        let row = l.bytes().map(parse_byte).collect::<Result<_, _>>()?;

        Ok(Self(row))
    }
}

/// A wrapper around the grid that implements `std::str::FromStr`;
#[derive(Debug, Default)]
struct Grid(Vec<Vec<Cell>>);

impl FromStr for Grid {
    type Err = ParseDigitError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid = s
            .lines()
            .map(|l| l.parse().map(|Row(r)| r))
            .collect::<Result<_, _>>()?;

        Ok(Self(grid))
    }
}

/// Solves the puzzle.
#[derive(Debug, Default)]
struct Solution {
    /// The height map.
    grid: Vec<Vec<Cell>>,
    /// The bounds of the height map.
    bounds: Point,

    /// The positions of all the low points.
    low_points: Vec<Point>,
    /// The solution to part 1: the sum of the risk levels of all low points.
    risk_level_sum: usize,
}

impl Solution {
    /// Finds the positions of all the low points.
    fn find_low_points(&mut self) {
        let Pair(r_max, c_max) = self.bounds;
        self.low_points = (0..r_max)
            .flat_map(|r| iter::repeat(r).zip(0..c_max))
            .flat_map(|t @ (r, c)| {
                let digit = self.grid[r][c];

                let point: Point = t.into();

                let low_point = point
                    .adjacent_cardinal()
                    .filter(|p| self.bounds.gt(p))
                    .all(|Pair(r, c)| self.grid[r][c] > digit);

                if low_point {
                    // UNWRAP: All digits are initialised as Option::Some; thus, it is fine to
                    // unwrap.
                    self.risk_level_sum += digit.unwrap() + 1;
                    Some(point)
                } else {
                    None
                }
            })
            .collect();
    }

    /// Returns a max-heap of basin sizes.
    fn basins(&mut self) -> BinaryHeap<usize> {
        mem::take(&mut self.low_points)
            .into_iter()
            .map(|low_point| {
                let mut len = 0;
                let mut adjacent = vec![low_point];

                while !adjacent.is_empty() {
                    adjacent = adjacent
                        .into_iter()
                        .flat_map(Pair::adjacent_cardinal)
                        .filter(|&p @ Pair(r, c)| {
                            self.bounds.gt(&p)
                                && self.grid[r][c] != Some(MAX_HEIGHT)
                                && ({
                                    let flag = self.grid[r][c].is_some();
                                    if flag {
                                        len += 1;
                                        self.grid[r][c] = None;
                                    }
                                    flag
                                })
                        })
                        .collect();
                }

                len
            })
            .collect()
    }

    /// Returns the product of the three largest basins in the heap.
    fn max_three_basins_product(&mut self) -> usize {
        self.basins().iter_heap().take(P2_BASINS).product()
    }
}

impl FromStr for Solution {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Grid(grid) = s.parse()?;
        let bounds = Pair(grid.len(), grid.get(0).ok_or(EmptyInputError)?.len());

        Ok(Self {
            grid,
            bounds,
            ..Default::default()
        })
    }
}

/// This puzzle.
struct D09;

impl Puzzle for D09 {
    type Solution = [usize; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut solution: Solution = input.parse()?;
        solution.find_low_points();
        Ok([solution.risk_level_sum, solution.max_three_basins_product()])
    }
}

aoc_2021::main!(D09);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D09.check("../Inputs/D09/sample.txt", [15, 1134]);
    }

    #[test]
    fn input() {
        D09.check("../Inputs/D09/input.txt", [512, 1_600_104]);
    }
}
