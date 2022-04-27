#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 11.](https://adventofcode.com/2021/day/11)

use std::str::FromStr;
use std::{iter, mem};

use crate::errors::EmptyInputError;
use crate::extension_traits::u8::U8Ext;
use crate::pair::{greater::Greater, Pair};
use crate::Puzzle;

/// The minimum energy level an octopus can have. This is `1` not `0` because an energy level of
/// `0` is represented by `Octopus::Flashed(_)`.
const MIN_STABLE_ENERGY: u16 = 1;

/// The maximum energy level an octopus can have.
const MAX_STABLE_ENERGY: u16 = 9;

/// Part 1: when to stop.
const BREAK_POINT: u16 = 100;

/// The position of an octopus.
type Position = Pair<usize>;

/// The energy level of an octopus.
#[derive(Debug)]
enum Octopus {
    /// `Octopus::Flashed(s)`: this octopus flashed (energy level 0) at step `s`.
    Flashed(u16),

    /// `Octopus::Stable(e)`: this octopus has an energy level of `e`.
    Stable(u16),
}

impl Octopus {
    /// Steps this octopus.
    ///
    /// If this octopus:
    ///
    /// - flashed this step => do nothing,
    /// - flashed previously => it now has an energy level of `MIN_STABLE_ENERGY`,
    /// - has an energy level of `MAX_STABLE_ENERGY` => it flashes this step,
    /// - has any other energy level, `e` => it now has an energy level of `e + 1`.
    fn step(&mut self, step: u16) {
        *self = match self {
            &mut Self::Flashed(s) if s == step => Self::Flashed(step),
            Self::Flashed(_) => Self::Stable(MIN_STABLE_ENERGY),
            Self::Stable(MAX_STABLE_ENERGY) => Self::Flashed(step),
            &mut Self::Stable(energy) => Self::Stable(energy + 1),
        };
    }

    /// Did this octopus flash?.
    fn flashed(&self) -> bool {
        matches!(self, &Self::Flashed(_))
    }
}

/// The simulation.
#[derive(Debug, Default)]
struct Simulation {
    /// The grid of octopuses.
    grid: Vec<Vec<Octopus>>,

    /// The size of the grid.
    size: Position,

    /// The current step in the simulation.
    step: u16,

    /// The total number of flashes since the start of the simulation.
    flashes: u16,

    /// The positions of the octopuses that flashed this step. The main purpose of this is to try
    /// to reuse the `Vec`'s allocation throughout the simulation.
    flash_positions: Vec<Position>,
}

impl Simulation {
    /// Returns `false` if the octopus at a given position has already flashed this step;
    /// otherwise, steps the octopus and checks if it flashes.
    fn flashes(&mut self, Pair(r, c): Position) -> bool {
        let current = &mut self.grid[r][c];
        match current {
            Octopus::Flashed(step) if *step == self.step => false,
            _ => {
                current.step(self.step);
                current.flashed()
            }
        }
    }

    /// Steps the simulation.
    fn step(&mut self) {
        let size = self.size;
        (0..size.0)
            .flat_map(|row| iter::repeat(row).zip(0..size.1))
            .map(Pair::from)
            .for_each(|position| {
                if self.flashes(position) {
                    let mut flashes = mem::take(&mut self.flash_positions);
                    flashes.push(position);
                    loop {
                        // CAST: from the assertion, A1, there are less than u16::MAX octopuses.
                        // Given that an octopus can only flash once per step, the number of
                        // flashes per step fits in a u16.
                        self.flashes += flashes.len() as u16;

                        // Step the adjacent octopuses and collect the positions of those that
                        // flash.
                        flashes = flashes
                            .into_iter()
                            .flat_map(Pair::adjacent)
                            .filter(|p| size.gt(p))
                            .flat_map(|adjacent| self.flashes(adjacent).then(|| adjacent))
                            .collect();

                        // Break when none of the adjacent octopuses flash.
                        if flashes.is_empty() {
                            break;
                        }
                    }

                    self.flash_positions = flashes;
                }
            });
        self.step += 1;
    }

    /// Checks if all octopuses flashed.
    fn synced(&self) -> bool {
        self.grid.iter().all(|r| r.iter().all(Octopus::flashed))
    }
}

impl FromStr for Simulation {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let grid = input
            .lines()
            .map(|l| {
                // CAST: a u8 fits in a u16.
                l.bytes()
                    .map(|b| b.parse_digit().map(|b| Octopus::Stable(b as u16)))
                    .collect()
            })
            .collect::<Result<Vec<Vec<_>>, _>>()?;

        let size = Pair(grid.len(), grid.get(0).ok_or(EmptyInputError)?.len());

        // ASSERTION: A1
        debug_assert!(size.into_iter().product::<usize>() < u16::MAX as usize);

        Ok(Self {
            grid,
            size,
            ..Default::default()
        })
    }
}

/// This puzzle.
pub struct D11;

impl Puzzle for D11 {
    type Solution = [u16; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut sim = input.parse::<Simulation>()?;

        let mut solution = Self::Solution::default();
        let mut reached_break_point = false;
        let mut grid_synced = false;

        loop {
            sim.step();

            if !reached_break_point && sim.step == BREAK_POINT {
                solution[0] = sim.flashes;
                reached_break_point = true;
            }

            if !grid_synced && sim.synced() {
                solution[1] = sim.step;
                grid_synced = true;
            }

            if reached_break_point && grid_synced {
                break;
            }
        }

        Ok(solution)
    }
}
