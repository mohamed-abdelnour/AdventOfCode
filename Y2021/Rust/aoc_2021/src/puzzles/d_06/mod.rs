//! [Day 06.](https://adventofcode.com/2021/day/6)

use std::{collections::VecDeque, ops::Range};

use crate::{errors::EmptyInputError, Puzzle};

/// The number of possible values a timer can have; the possible values are 0 to 8 (inclusive);
/// thus, 9 possibilities.
const TIMERS: usize = 9;

/// Part 1 stops after day 80.
const BREAK_POINT_1: usize = 80;

/// Part 2 stops after day 256.
const BREAK_POINT_2: usize = 256;

/// The frequency of each timer value at the current state.
//
// # Complexity
//
// This uses a `VecDeque` instead of an array, for example, to get more efficient `rotate_left`.
//
// Each day `self.0.rotate_left(1)` is called to update the state: using a `VecDeque`, this takes
// `O(1)` time; a slice, on the other hand, would have taken linear (in the length, `TIMERS`) time.
struct Count(VecDeque<u64>);

impl Count {
    /// Simulates one day.
    // Each day, all timers (except 0) are decremented by one and any timer that reaches 0 spawns a
    // new child with timer = 8. This is simulated by rotating the deque once to the left.
    // All timers that reach zero are also reset to 6 at the end of the day. This is simulated by
    // incrementing the count at 6 by the count at 8 (which was previously at 0 before the
    // rotation).
    fn step(&mut self) {
        let Count(count) = self;
        count.rotate_left(1);
        count[6] += count[8];
    }

    /// Simulates days in `range`.
    fn simulate(&mut self, range: Range<usize>) {
        range.for_each(|_| self.step());
    }

    /// Counts the fish.
    fn count(&self) -> u64 {
        self.0.iter().sum()
    }
}

impl FromIterator<usize> for Count {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = usize>,
    {
        let mut count = VecDeque::with_capacity(TIMERS);
        count.resize(TIMERS, 0);
        iter.into_iter().for_each(|index| {
            count[index] += 1;
        });
        Self(count)
    }
}

/// This puzzle.
pub struct D06;

impl Puzzle for D06 {
    type Solution = [u64; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut count = input
            .lines()
            .next()
            .ok_or(EmptyInputError)?
            .split(',')
            .map(str::parse)
            .collect::<Result<Count, _>>()?;

        let mut results = [0; 2];

        count.simulate(0..BREAK_POINT_1);
        results[0] = count.count();

        count.simulate(BREAK_POINT_1..BREAK_POINT_2);
        results[1] = count.count();

        Ok(results)
    }
}
