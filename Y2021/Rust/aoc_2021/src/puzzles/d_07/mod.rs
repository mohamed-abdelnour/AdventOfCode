#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 07.](https://adventofcode.com/2021/day/7)

use std::cmp::Ordering;

use crate::errors::EmptyInputError;
use crate::search::{Binary, Search};
use crate::{define_error, Puzzle};

define_error!(NoMinimum, "could not find a unique minimum value");

/// An alias for the function pointer type for each sub-part.
type Part = fn(&mut State, usize) -> usize;

/// Metadata for the list of numbers.
#[derive(Clone, Debug, Default)]
struct Meta {
    /// The number of the values in the list that are smaller than the current one.
    length: usize,
    /// The sum of the values in the list that are smaller than the current one.
    sum: usize,
    /// The sum of the absolute differences between each number in the list and the current one.
    sad: Option<usize>,
    /// The sum of the arithmetic series with `a = 1`, `d = 1` and `a_n = sad`.
    sasad: Option<usize>,
}

impl Meta {
    /// Construct a new `Meta` given the length of numbers smaller than the current one and their
    /// sum.
    fn new(length: usize, sum: usize) -> Self {
        Self {
            length,
            sum,
            ..Default::default()
        }
    }
}

/// A vector of all the metadata needed and other constants.
#[derive(Debug)]
struct State {
    /// For a list, `l`, this holds metadata for all indices in the range
    /// `l.iter().min()..=l.iter().max()`.
    points: Vec<Meta>,
    /// The length of the original list of numbers.
    length: usize,
    /// The sum of all the numbers in the original list.
    sum: usize,
    /// The sum of the squares of all the numbers in the original list.
    sum_squares: usize,
}

// The intuition behind this is explained in docs/d_07.pdf.
impl State {
    /// Returns the sum of absolute differences between all the numbers in the list and `k`.
    // Equation P in docs/d_07.pdf.
    fn sad(&mut self, k: usize) -> usize {
        let point = &mut self.points[k];
        if let Some(sad) = point.sad {
            sad
        } else {
            let sad = self.sum + 2 * point.length * k - (2 * point.sum + self.length * k);
            point.sad = Some(sad);
            sad
        }
    }

    /// Returns the sum of the arithmetic series with `a = 1`, `d = 1` and `a_n = self.sad(k)`.
    // Equation Q in docs/d_07.pdf.
    fn sasad(&mut self, k: usize) -> usize {
        let sasad = (self.sad(k) + self.sum_squares + self.length * k.pow(2)) / 2 - k * self.sum;
        self.points[k].sasad = Some(sasad);
        sasad
    }

    /// Finds the minimum value.
    ///
    /// # Complexity
    ///
    /// This gives the same result as `(0..self.points.len()).map(|i| f(self, i)).min()`; however,
    /// this takes advantage of the convexity of `f` over the domain by using binary search to find
    /// the minimum.
    fn min_by(&mut self, f: Part) -> Option<usize> {
        // Given that `f` is convex over the domain:

        // 1. If the first value is smaller than the one after it, then it is the minimum.
        let first = 0;
        let mut this = f(self, first);
        if this < f(self, first + 1) {
            return Some(this);
        }

        // 2. If the last value is smaller than the one before it, then it is the minimum.
        let last = self.points.len() - 1;
        this = f(self, last);
        if this < f(self, last - 1) {
            return Some(this);
        }

        // 3. Otherwise, use binary search to find the minimum starting from the second value up to
        //    the second to last one.
        Binary::new(first + 1, last)
            .search_by(|k| {
                this = f(self, k);
                let left = f(self, k - 1);
                let right = f(self, k + 1);

                // Compare each value to its neighbours, moving to the left if the function is
                // increasing and moving to the right if the function is decreasing until the
                // minimum is found.
                if right < this && this < left {
                    Ordering::Less
                } else if left < this && this < right {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .map(|_| this)
            .ok()
    }

    /// Finds the minimum values for the two parts of the puzzle.
    fn solve(&mut self) -> Option<[usize; 2]> {
        Some([self.min_by(Self::sad)?, self.min_by(Self::sasad)?])
    }
}

impl From<Vec<usize>> for State {
    fn from(mut numbers: Vec<usize>) -> Self {
        numbers.sort_unstable();

        let length = numbers.len();

        // This needs to contain metadata for all the numbers, `n`, in `(0..=numbers.len())`: if
        // `n` does not occur in `numbers`, then its metadata is the same as that of the closest
        // number to it that is both smaller than it and occurs in `numbers`.
        let mut points = Vec::with_capacity(numbers[length - 1] + 1);

        let mut sum = 0;
        let mut sum_squares = 0;

        numbers.iter().enumerate().for_each(|(i, n)| {
            sum += n;
            sum_squares += n.pow(2);

            let point = Meta::new(i + 1, sum);
            if let Some(next) = numbers.get(i + 1) {
                // Fill the missing values at the last occurrence of `n`.
                if next != n {
                    points.resize(*next, point);
                }
            } else {
                // Fill the last value.
                points.resize(n + 1, point);
            }
        });

        Self {
            points,
            length,
            sum,
            sum_squares,
        }
    }
}

impl FromIterator<usize> for State {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = usize>,
    {
        iter.into_iter().collect::<Vec<_>>().into()
    }
}

/// This puzzle.
pub struct D07;

impl Puzzle for D07 {
    type Solution = [usize; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        input
            .lines()
            .next()
            .ok_or(EmptyInputError)?
            .split(',')
            .map(str::parse)
            .collect::<Result<State, _>>()?
            .solve()
            .ok_or_else(|| NoMinimum.into())
    }
}
