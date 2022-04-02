#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 05.](https://adventofcode.com/2021/day/5)

use std::num::TryFromIntError;
use std::str::FromStr;

use aoc_2021::integer::Integer;
use aoc_2021::pair::Pair;
use aoc_2021::{define_error, Puzzle};

define_error!(
    PositionFormatError,
    "the coordinates of a position should be separated by a comma"
);

define_error!(
    SegmentFormatError,
    r#"the start and end positions should be separated by a " -> ""#
);

/// An alias for `Result<T, TryFromIntError>`.
type TryFromIntResult<T> = Result<T, TryFromIntError>;

/// The position of a vent.
type Point = Pair<i16>;

/// Extension trait to convert a `&str` to a `Pair`.
trait ToPair {
    /// Performs the conversion.
    fn to_pair(&self) -> anyhow::Result<Point>;
}

impl ToPair for &str {
    fn to_pair(&self) -> anyhow::Result<Point> {
        let (x, y) = self.split_once(',').ok_or(PositionFormatError)?;
        Ok(Pair(x.parse()?, y.parse()?))
    }
}

/// The positions of all vents in this line.
#[derive(Debug)]
struct Segment {
    /// The start position, obtained from parsing the list.
    start: Point,
    /// The end position, obtained from parsing the list.
    end: Point,
    /// The smallest Point that divides the segment.
    step: Point,
    /// The number of points in this segment.
    size: usize,
}

impl Segment {
    /// Creates a new segment after parsing a line from the list.
    fn new(start: Point, end: Point) -> Self {
        let Pair(x, y) = end - start;
        let gcd = x.gcd(&y);
        let simplify = |n| n / gcd;
        let dx = simplify(x);
        let dy = simplify(y);

        let step = Pair(dx, dy);

        let size = {
            let size_semi_exclusive = {
                if dx == 0 {
                    (end.1 - start.1) / dy
                } else {
                    (end.0 - start.0) / dx
                }
            };

            // CAST: size_semi_exclusive is guaranteed to be non-negative, and it is also
            // guaranteed to fit in a usize; thus, it is fine to cast.
            size_semi_exclusive as usize + 1
        };

        Self {
            start,
            end,
            step,
            size,
        }
    }

    /// Returns the upper bounds of the segment.
    fn upper_bounds(&self) -> Point {
        let Pair(rs, cs) = self.start;
        let Pair(re, ce) = self.end;
        Pair(rs.max(re), cs.max(ce))
    }

    /// Checks whether this segment is either vertical or horizontal or not.
    fn is_straight(&self) -> bool {
        let Pair(a, b) = self.start;
        let Pair(p, q) = self.end;
        a == p || b == q
    }

    /// Increments the number of overlaps for each point in this segment and the count of overlaps
    /// that are greater than or equal to 2.
    fn count(mut self, counts: &mut Vec<Vec<usize>>, count: &mut usize) -> TryFromIntResult<()> {
        self.try_for_each(|Pair(x, y)| {
            let x: usize = x.try_into()?;
            let y: usize = y.try_into()?;

            let overlaps = &mut counts[x][y];
            *overlaps += 1;
            if *overlaps == 2 {
                *count += 1;
            }

            Ok(())
        })
    }
}

impl FromStr for Segment {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split_once(" -> ").ok_or(SegmentFormatError)?;
        let start = a.to_pair()?;
        let end = b.to_pair()?;
        Ok(Self::new(start, end))
    }
}

impl Iterator for Segment {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.size == 0 {
            None
        } else {
            self.size -= 1;
            let current = self.start;
            self.start += self.step;
            Some(current)
        }
    }
}

/// This puzzle.
struct D05;

impl Puzzle for D05 {
    type Solution = [usize; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut result = Ok(());

        // The upper bounds of all the segments.
        let Pair(row, column) = &mut Point::default();

        let (straight, slanted): (Vec<_>, Vec<_>) = input
            .lines()
            .scan((), |_, line| {
                line.parse::<Segment>().map_or_else(
                    // Capture the error and short-circuit.
                    |err| {
                        result = Err(err);
                        None
                    },
                    |segment| {
                        // Update the bounds.
                        let Pair(r, c) = segment.upper_bounds();
                        *row = (*row).max(r);
                        *column = (*column).max(c);

                        // Yield the current segment.
                        Some(segment)
                    },
                )
            })
            .partition(Segment::is_straight);

        // Short-circuit if an error occurred.
        result?;

        // Make the bounds inclusive and initialise the counts matrix.
        let rows = (*row + 1).try_into()?;
        let columns = (*column + 1).try_into()?;
        let mut counts = vec![vec![0; columns]; rows];

        let mut p_1 = 0;
        straight
            .into_iter()
            .try_for_each(|s| s.count(&mut counts, &mut p_1))?;

        let mut p_2 = p_1;
        slanted
            .into_iter()
            .try_for_each(|s| s.count(&mut counts, &mut p_2))?;

        Ok([p_1, p_2])
    }
}

aoc_2021::main!(D05);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D05.check("../Inputs/D05/sample.txt", [5, 12]);
    }

    #[test]
    fn input() {
        D05.check("../Inputs/D05/input.txt", [5_608, 20_299]);
    }
}
