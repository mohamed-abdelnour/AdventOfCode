#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 08.](https://adventofcode.com/2021/day/8)

use std::str::FromStr;

use aoc_2021::bits::{Bit, Bits};
use aoc_2021::iterator::Array;
use aoc_2021::pair::Pair;
use aoc_2021::transpose::ArrayBitTranspose;
use aoc_2021::{define_error, Puzzle};

/// A helper module that was used to encode each digit, and find the number of ones per encoded
/// digit. This is kept only for reference, and is not included in the resulting binary when
/// compiling with optimisations turned on.
#[cfg(debug_assertions)]
mod dev;

/// The delimiter separating the input and output digits.
const DELIMITER: &str = " | ";

/// The number of possible digits: 0..=9.
const DIGITS: usize = 10;

/// The number of possible segments: A..=G.
const SEGMENTS: usize = 7;

/// The number of digits in the output value.
const OUTPUT: usize = 4;

define_error!(
    ParseSegmentError,
    r#"a segment has to be a single character in "('a'..='g')""#
);

define_error!(
    DisplayDelimiterError,
    r#"the signal patterns and the output value must be delimited by " | ""#
);

/// The possible variants of a segment.
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Debug)]
enum Segment {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}

impl Segment {
    /// Encodes each segment variant as follows:
    ///
    /// ```rust
    /// Segment::A => 1,
    /// Segment::B => 2,
    /// Segment::C => 4,
    /// Segment::D => 8,
    /// Segment::E => 16,
    /// Segment::F => 32,
    /// Segment::G => 64,
    /// ```
    fn encode(self) -> usize {
        // CAST: the enum fits in an u32.
        2_usize.pow(self as u32)
    }
}

impl TryFrom<char> for Segment {
    type Error = ParseSegmentError;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            'a' => Ok(Segment::A),
            'b' => Ok(Segment::B),
            'c' => Ok(Segment::C),
            'd' => Ok(Segment::D),
            'e' => Ok(Segment::E),
            'f' => Ok(Segment::F),
            'g' => Ok(Segment::G),
            _ => Err(ParseSegmentError),
        }
    }
}

/// A wrapper around an encoded digit.
#[derive(Debug, Default)]
struct Digit(usize);

impl Digit {
    /// Decodes the encoded digit, as calculated in `dev::encode()`.
    fn decode(self) -> usize {
        match self.0 {
            119 => 0,
            18 => 1,
            93 => 2,
            91 => 3,
            58 => 4,
            107 => 5,
            111 => 6,
            82 => 7,
            127 => 8,
            123 => 9,
            _ => unreachable!(),
        }
    }
}

impl FromStr for Digit {
    type Err = ParseSegmentError;

    fn from_str(digit: &str) -> Result<Self, Self::Err> {
        // Encode each segment in the digit and add them up to get a unique representation for this
        // digit. This used try_fold() instead of sum() to account for the possibility of encoding
        // a char to fail (which will not happen given a correct input).
        let digit = digit
            .chars()
            .try_fold(0, |acc, c| Segment::try_from(c).map(|s| acc + s.encode()))?;
        Ok(Self(digit))
    }
}

/// The critical digits used in identifying how to fix the connection. These are the digits with a
/// unique number of ones in their encoded representations (other than the eight which is all ones,
/// and thus not particularly useful).
///
/// These are mapped as follows:
///
/// ```txt
/// 2 ones => 1,
/// 3 ones => 7,
/// 4 ones => 4,
/// ```
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Debug, Default)]
struct Critical {
    one: usize,
    seven: usize,
    four: usize,
}

/// A wrapper that holds the input digits and populates the critical digits as they are found.
struct Digits {
    /// The ten, unique input digits.
    digits: [usize; DIGITS],
    /// The three critical digits.
    critical: Critical,
}

impl FromStr for Digits {
    type Err = ParseSegmentError;

    fn from_str(ds: &str) -> Result<Self, Self::Err> {
        let mut critical: Critical = Default::default();

        let digits = ds
            .split_ascii_whitespace()
            .map(|digit| -> Result<_, ParseSegmentError> {
                let Digit(digit) = digit.parse()?;

                match digit.count_ones() {
                    2 => critical.one = digit,
                    3 => critical.seven = digit,
                    4 => critical.four = digit,
                    _ => {}
                }

                Ok(digit)
            })
            .collect::<Result<Array<_, DIGITS>, _>>()?
            .0;

        Ok(Self { digits, critical })
    }
}

/// A wrapper that maps the faulty connections to the correct ones.
///
/// # Examples
///
/// An array of `[6, 5, 4, 3, 0, 1, 2]` means that for each segment in this display:
///
/// ```rust
/// Segment::A => Segment::G,
/// Segment::B => Segment::F,
/// Segment::C => Segment::E,
/// Segment::D => Segment::D,
/// Segment::E => Segment::A,
/// Segment::F => Segment::B,
/// Segment::G => Segment::C
/// ```
struct Fixes([usize; SEGMENTS]);

impl From<Digits> for Fixes {
    // Given the correct display connections, each digit is represented as follows:
    //
    // const DISPLAY: [[usize; SEGMENTS]; DIGITS] = [
    //     //       g, f, e, d, c, b, a
    //     /* 0 */ [1, 1, 1, 0, 1, 1, 1],
    //     /* 1 */ [0, 1, 0, 0, 1, 0, 0],
    //     /* 2 */ [1, 0, 1, 1, 1, 0, 1],
    //     /* 3 */ [1, 1, 0, 1, 1, 0, 1],
    //     /* 4 */ [0, 1, 0, 1, 1, 1, 0],
    //     /* 5 */ [1, 1, 0, 1, 0, 1, 1],
    //     /* 6 */ [1, 1, 1, 1, 0, 1, 1],
    //     /* 7 */ [0, 1, 0, 0, 1, 0, 1],
    //     /* 8 */ [1, 1, 1, 1, 1, 1, 1],
    //     /* 9 */ [1, 1, 0, 1, 1, 1, 1],
    //     // ones: 7, 9, 4, 7, 8, 6, 8
    // ];
    //
    // The segments B, E and F have a unique number of ones, and thus can be directly identified.
    //
    // The segments A and C both have 8 ones; they are identified by comparing the faulty
    // representations of the critical digits 1 and 7. If 1 does not have this segment but 7 has
    // it, then this should map to A; otherwise, it should map to C.
    //
    // The segments D and G both have 7 ones; they are identified by comparing the faulty
    // representations of the critical digits 1 and 4. If 1 does not have this segment but 4 has
    // it, then this should map to D; otherwise, it should map to G.
    fn from(Digits { digits, critical }: Digits) -> Self {
        let mut fixes = digits.bit_transpose::<SEGMENTS>();
        fixes.iter_mut().enumerate().for_each(|(i, n)| {
            let m = match n.count_ones() {
                6 => Segment::B,
                4 => Segment::E,
                9 => Segment::F,
                8 => {
                    if critical.one.bit(i) == Bit::Zero && critical.seven.bit(i) == Bit::One {
                        Segment::A
                    } else {
                        Segment::C
                    }
                }
                7 => {
                    if critical.one.bit(i) == Bit::Zero && critical.four.bit(i) == Bit::One {
                        Segment::D
                    } else {
                        Segment::G
                    }
                }
                _ => unreachable!(),
            };

            // CAST: the enum fits in a usize.
            *n = m as usize;
        });

        Self(fixes)
    }
}

/// A wrapper around the four-digit output and the solution to both parts of the puzzle.
struct Output {
    /// The four-digit output.
    output: [usize; OUTPUT],
    /// The puzzle solution.
    solution: Pair<usize>,
}

impl Output {
    /// Solves part 2.
    fn solve(&mut self, Fixes(fixes): Fixes) {
        let transposed = self.output.bit_transpose::<SEGMENTS>();
        let mut transposed_fixed: [usize; SEGMENTS] = Default::default();

        fixes.into_iter().enumerate().for_each(|(old, new)| {
            transposed_fixed[new] = transposed[old];
        });

        // Part 2: decode each digit in the array and "concatenate" the output value.
        //
        // For example, for transposed_fixed.bit_transpose::<OUTPUT>() = [91, 107, 91, 107]:
        //
        // Digit(107).decode() == 5,
        // Digit(91).decode() == 3,
        //
        // ((((0 * 10 + 5) * 10 + 3) * 10 + 5) * 10 + 3) = 5353.
        self.solution.1 = transposed_fixed
            .bit_transpose::<OUTPUT>()
            .into_iter()
            .rfold(0, |acc, n| acc * 10 + Digit(n).decode());
    }
}

impl FromStr for Output {
    type Err = ParseSegmentError;

    fn from_str(os: &str) -> Result<Self, Self::Err> {
        let mut solution: Pair<usize> = Default::default();

        let output = os
            .split_ascii_whitespace()
            .map(|digit| -> Result<_, ParseSegmentError> {
                let Digit(digit) = digit.parse()?;

                // Part 1: update the count for each digit with a unique number of ones.
                if matches!(digit.count_ones(), 2 | 3 | 4 | 7) {
                    solution.0 += 1;
                }

                Ok(digit)
            })
            .collect::<Result<Array<_, OUTPUT>, _>>()?
            .0;

        Ok(Self { output, solution })
    }
}

/// A wrapper around the puzzle solution that is used to convert from puzzle input to the solution.
#[derive(Debug)]
struct Display(Pair<usize>);

impl FromStr for Display {
    type Err = anyhow::Error;

    fn from_str(display: &str) -> Result<Self, Self::Err> {
        let (ds, os) = display.split_once(DELIMITER).ok_or(DisplayDelimiterError)?;
        let digits: Digits = ds.parse()?;
        let mut output: Output = os.parse()?;
        output.solve(digits.into());
        Ok(Self(output.solution))
    }
}

/// This puzzle.
struct D08;

impl Puzzle for D08 {
    type Solution = [usize; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        // If running in debug mode, encode the digits and count the number of ones per encoded
        // digit.
        #[cfg(debug_assertions)]
        {
            dev::encode();
            println!();
        }

        input
            .lines()
            .map(str::parse)
            .try_fold(Pair::default(), |acc, display| {
                display.map(|Display(display)| acc + display)
            })
            .map(Pair::into)
    }
}

aoc_2021::main!(D08);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D08.check("../Inputs/D08/sample.txt", [26, 61_229]);
    }

    #[test]
    fn input() {
        D08.check("../Inputs/D08/input.txt", [416, 1_043_697]);
    }
}

#[cfg(test)]
mod private {
    use super::*;

    #[test]
    fn sample() {
        const S: &str =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

        let Display(Pair(_, x)) = S.parse().unwrap();

        assert_eq!(x, 5353);
    }
}
