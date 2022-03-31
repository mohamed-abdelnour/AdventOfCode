#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 03.](https://adventofcode.com/2021/day/3)

use std::convert::identity;
use std::ops::Not;

use aoc_2021::bits::{Bit, Bits};
use aoc_2021::errors::EmptyInputError;
use aoc_2021::pair::Pair;
use aoc_2021::Puzzle;

/// A counter for the number of zeros and that of ones, respectively.
#[derive(Debug, Default)]
struct Count {
    /// The number of zeros.
    zeros: usize,
    /// The number of ones.
    ones: usize,
}

impl Count {
    /// Updates the frequency count given a bit.
    fn update(&mut self, other: Bit) {
        match other {
            Bit::Zero => self.zeros += 1,
            Bit::One => self.ones += 1,
        }
    }

    /// Returns the most common bit after counting; returns `Bit::One` if both are equally common.
    fn most_common(&self) -> Bit {
        Bit::from(self.ones >= self.zeros)
    }
}

/// An extension trait that provides `most_common_at` for a type.
trait Input {
    /// Computes the most common bit at a given position in a slice of numbers.
    fn most_common_at(self, index: u32) -> Bit;
}

impl Input for &[u32] {
    fn most_common_at(self, index: u32) -> Bit {
        let mut count = Count::default();
        let mid = self.len() / 2;
        for number in self {
            // Given that there are _exactly_ 2 possible values for a bit: if either bit occurs
            // more than `mid` times, then it is guaranteed to be the most common one.
            if count.zeros > mid || count.ones > mid {
                break;
            }
            count.update(number.bit(index));
        }
        count.most_common()
    }
}

/// The puzzle parameters.
type Parameters = Pair<u32>;

/// Part 1.
#[derive(Debug)]
struct P1<'a> {
    /// Gamma and epsilon.
    ps: Parameters,
    /// The puzzle input.
    input: &'a [u32],
}

impl P1<'_> {
    /// Steps the input, calculating the most common bit for a given index and updating the gamma
    /// end epsilon accordingly.
    /// The return value is only useful for the first step as it is later used in partitioning the
    /// input for part 2.
    fn step(&mut self, index: u32) -> Bit {
        let most_common = self.input.most_common_at(index);
        let multiple = 2u32.pow(index);
        if most_common == Bit::One {
            self.ps.0 += multiple;
        } else {
            self.ps.1 += multiple;
        }
        most_common
    }
}

/// Part 2.
#[derive(Debug)]
struct P2 {
    /// A transformation to the most common value at a given position that determines which numbers
    /// to retain.
    f: fn(Bit) -> Bit,
    /// An owned copy of the puzzle input that gets shrunk until only one element remains.
    input: Vec<u32>,
}

impl P2 {
    /// Returns an instance of `P2` that does not transform the most common value at a given
    /// position.
    fn oxygen(input: Vec<u32>) -> Self {
        Self { f: identity, input }
    }

    /// Returns an instance of `P2` that transforms the most common value at a given position to
    /// get the least common one.
    fn carbon_dioxide(input: Vec<u32>) -> Self {
        Self { f: Not::not, input }
    }

    /// Retains the elements that have a bit at the current position that matches either the most
    /// common or the least common one (according to `self.f`) in all elements at this position.
    fn step(&mut self, index: u32) {
        if self.input.len() != 1 {
            let keep = (self.f)(self.input.most_common_at(index));
            self.input.retain(|number| number.bit(index) == keep);
        }
    }
}

/// This puzzle.
struct D03;

impl Puzzle for D03 {
    type Solution = [u32; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let first = input.lines().next().ok_or(EmptyInputError)?;
        let last_index: u32 = (first.chars().count() - 1).try_into()?;
        let input = input
            .lines()
            .map(|s| u32::from_str_radix(s, 2))
            .collect::<Result<Vec<_>, _>>()?;

        // Part 1: step the input once.
        let ps = Parameters::default();
        let mut gamma_epsilon = P1 { ps, input: &input };
        let most_common = gamma_epsilon.step(last_index);

        // Part 2: use the value for `most_common` computed during the first step to partition
        // input into 2 parts.
        let (oxygen, carbon_dioxide) = input
            .iter()
            .partition(|number| number.bit(last_index) == most_common);

        let mut oxygen = P2::oxygen(oxygen);
        let mut carbon_dioxide = P2::carbon_dioxide(carbon_dioxide);

        (0..last_index).rev().for_each(|index| {
            gamma_epsilon.step(index);
            oxygen.step(index);
            carbon_dioxide.step(index);
        });

        let p_1 = gamma_epsilon.ps;
        let p_2 = Pair(oxygen.input[0], carbon_dioxide.input[0]);
        Ok([p_1.product(), p_2.product()])
    }
}

aoc_2021::main!(D03);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D03.check("../Inputs/D03/sample.txt", [198, 230]);
    }

    #[test]
    fn input() {
        D03.check("../Inputs/D03/input.txt", [3_320_834, 4_481_199]);
    }
}

#[cfg(test)]
mod private {
    use super::*;

    #[test]
    fn most_common_even() {
        let all_zeros = [0, 0, 0, 0].most_common_at(0);
        assert_eq!(all_zeros, Bit::Zero);

        let all_ones = [1, 1, 1, 1].most_common_at(0);
        assert_eq!(all_ones, Bit::One);

        let most_zeros = [0, 0, 0, 1].most_common_at(0);
        assert_eq!(most_zeros, Bit::Zero);

        let most_ones = [1, 1, 0, 1].most_common_at(0);
        assert_eq!(most_ones, Bit::One);

        let equally_common = [0, 0, 1, 1].most_common_at(0);
        assert_eq!(equally_common, Bit::One);
    }

    #[test]
    fn most_common_odd() {
        let all_zeros = [0, 0, 0].most_common_at(0);
        assert_eq!(all_zeros, Bit::Zero);

        let all_ones = [1, 1, 1].most_common_at(0);
        assert_eq!(all_ones, Bit::One);

        let most_zeros = [0, 0, 1].most_common_at(0);
        assert_eq!(most_zeros, Bit::Zero);

        let most_ones = [1, 0, 1].most_common_at(0);
        assert_eq!(most_ones, Bit::One);
    }
}
