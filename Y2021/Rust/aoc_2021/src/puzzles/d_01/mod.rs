#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 01.](https://adventofcode.com/2021/day/1)

use crate::{errors::EmptyInputError, Puzzle};

/// An accumulator of the number of times the measurements increase.
#[derive(Debug, Default)]
struct State {
    /// The number of times the measurements increase.
    count: u16,
    /// The current measurement.
    current: u16,
}

impl State {
    /// Creates a new `State` given an initial element.
    fn new(current: u16) -> Self {
        State {
            current,
            ..Default::default()
        }
    }

    /// Compares the current measurement to the previous one and updates the count accordingly.
    fn step(&mut self, current: u16) {
        if current > self.current {
            self.count += 1;
        }
        self.current = current;
    }
}

/// This puzzle.
pub struct D01;

impl Puzzle for D01 {
    type Solution = [u16; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let input: Vec<_> = input.lines().map(str::parse).collect::<Result<_, _>>()?;

        // The initial measurement.
        let measurement = *input.get(0).ok_or(EmptyInputError)?;
        let mut measurement = State::new(measurement);

        // Split the measurements into the initial three-measurement window and the rest of the
        // measurements, and calculate the sum of that initial window.
        let (three_window, offset) = input.split_at(3);
        let mut three_window = State::new(three_window.iter().sum());
        let mut offset = offset.iter();

        let mut next_three_window = u16::default();

        let step = |&next_measurement| {
            if let Some(next_offset) = offset.next() {
                // The next three-measurement window sum is the current three-measurement window sum
                // plus the next measurement in the `offset` iterator minus the current measurement.
                next_three_window = three_window.current + next_offset - measurement.current;
                three_window.step(next_three_window);
            }
            measurement.step(next_measurement);
        };

        // Walk the input, applying `step` for each item after the initial measurement.
        input.iter().skip(1).for_each(step);

        Ok([measurement.count, three_window.count])
    }
}
