#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 16.](https://adventofcode.com/2021/day/16)

use std::str::FromStr;

use crate::bits::{iterator::BitIter, Bit};
use crate::errors::EmptyInputError;
use crate::integer::Integer;
use crate::{define_error, Puzzle};

/// The number of bits used to represent a hexadecimal digit.
const BITS: u8 = 4;
/// The radix of a hexadecimal digit.
const RADIX: u32 = 16;

/// Half of the bits used to represent a packet's header, i.e. the number of bits used to represent
/// a packet's version or type ID.
const HALF_HEADER: u8 = 3;

/// For operator packets with length type ID 0: the number of bits representing the total length of
/// the sub-packets contained by this packet.
const LENGTH: u8 = 15;
/// For operator packets with length type ID 1: the number of sub-packets immediately contained by
/// this packet.
const NUMBER: u8 = 11;

define_error!(
    ParseHexError,
    "a hexadecimal digit must be in '0'..='9', 'A'..='F' or 'a'..='f'"
);

define_error!(EvalPacketError, "could not evaluate the packet");

/// An interface for comparing two items in a collection.
trait CmpTwo<T> {
    /// Returns the result of comparing two items in the collection as a `T`.
    fn cmp_two<F>(self, f: F) -> Option<T>
    where
        T: PartialOrd + From<bool>,
        F: Fn(&T, &T) -> bool;
}

impl<T> CmpTwo<T> for Vec<T> {
    fn cmp_two<F>(self, f: F) -> Option<T>
    where
        T: PartialOrd + From<bool>,
        F: Fn(&T, &T) -> bool,
    {
        self.get(0)
            .and_then(|a| self.get(1).map(move |b| f(a, b).into()))
    }
}

/// An alias for a function pointer type that reduces a stack using an operation.
type Eval = fn(Vec<u64>) -> Option<u64>;

/// Defines an type that evaluates a stack given a reducer.
macro_rules! eval {
    ($struct:ident => $fn:expr) => {
        struct $struct;

        impl From<$struct> for Eval {
            fn from(_: $struct) -> Self {
                $fn
            }
        }
    };
}

eval!(Zero => |x| Some(x.into_iter().sum()));
eval!(One => |x| Some(x.into_iter().product()));
eval!(Two => |x| x.into_iter().min());
eval!(Three => |x| x.into_iter().max());
eval!(Five => |x| x.cmp_two(PartialOrd::gt));
eval!(Six => |x| x.cmp_two(PartialOrd::lt));
eval!(Seven => |x| x.cmp_two(PartialEq::eq));

/// A hexadecimal number.
#[derive(Debug, Default)]
struct Hex {
    /// The underlying decimal number.
    x: u64,
}

impl From<u64> for Hex {
    fn from(x: u64) -> Self {
        Hex { x }
    }
}

impl TryFrom<char> for Hex {
    type Error = ParseHexError;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        c.to_digit(RADIX)
            .map(|d| u64::from(d).into())
            .ok_or(ParseHexError)
    }
}

impl IntoIterator for Hex {
    type Item = Bit;

    type IntoIter = BitIter<u64>;

    fn into_iter(self) -> Self::IntoIter {
        BitIter::new(self.x, u32::from(BITS))
    }
}

/// A packet.
#[derive(Debug, Default)]
pub struct Packet {
    /// The bits in the packet.
    payload: Vec<Bit>,

    /// The sum of the version numbers in all packets.
    pub sum: u64,
}

impl Packet {
    /// Removes the `n` most-significant bits in the packet and returns their decimal value.
    fn convert<N: Integer>(&mut self, n: u8) -> Option<N> {
        (0..n).try_fold(N::ZERO, |acc, _| {
            self.payload
                .pop()
                .map(move |bit| (acc << N::ONE) + N::from(bit))
        })
    }

    /// Extracts the literal contained in a literal value packet.
    fn literal(&mut self) -> Option<u64> {
        let mut literal = 0;
        loop {
            let prefix = self.payload.pop()?;

            literal <<= BITS;
            literal += self.convert::<u64>(BITS)?;

            if let Bit::Zero = prefix {
                break;
            }
        }
        Some(literal)
    }

    /// Parses an operator packet and evaluates it using `op`.
    fn operator(&mut self, op: impl Into<Eval>) -> Option<u64> {
        let mut stack;

        match self.payload.pop()? {
            Bit::Zero => {
                stack = Vec::new();
                let length = self.convert::<usize>(LENGTH)?;
                let enough = self.payload.len() - length;

                while self.payload.len() != enough {
                    stack.push(self.eval()?);
                }
            }

            Bit::One => {
                let number = self.convert(NUMBER)?;
                stack = (0..number).map(|_| self.eval()).collect::<Option<_>>()?;
            }
        }

        op.into()(stack)
    }

    /// Evaluates a packet.
    pub fn eval(&mut self) -> Option<u64> {
        self.sum += self.convert::<u64>(HALF_HEADER)?;
        let id = self.convert(HALF_HEADER)?;

        match id {
            4 => self.literal(),

            0 => self.operator(Zero),
            1 => self.operator(One),
            2 => self.operator(Two),
            3 => self.operator(Three),
            5 => self.operator(Five),
            6 => self.operator(Six),
            7 => self.operator(Seven),

            // All the values that can be represented in a 3-bit unsigned integer are covered.
            _ => unreachable!(),
        }
    }
}

impl FromIterator<Hex> for Packet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Hex>,
    {
        Self {
            payload: iter.into_iter().flatten().collect(),
            ..Default::default()
        }
    }
}

impl FromStr for Packet {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .next()
            .ok_or(EmptyInputError)?
            .chars()
            .rev()
            .map(Hex::try_from)
            .collect::<Result<Vec<_>, _>>()
            .map(|payload| payload.into_iter().collect())
            .map_err(Into::into)
    }
}

/// This puzzle.
pub struct D16;

impl Puzzle for D16 {
    type Solution = [u64; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut packet = input.parse::<Packet>()?;
        let value = packet.eval().ok_or(EvalPacketError)?;
        Ok([packet.sum, value])
    }
}
