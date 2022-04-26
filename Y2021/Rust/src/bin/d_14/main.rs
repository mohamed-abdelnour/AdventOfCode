#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 14.](https://adventofcode.com/2021/day/14)

use std::array::TryFromSliceError;
use std::collections::HashMap;
use std::mem;
use std::str::FromStr;

use aoc_2021::extension_traits::str::StrExt;
use aoc_2021::{define_error, Puzzle};

/// Apply 10 steps for part 1.
const BREAK_POINT_1: u8 = 10;

/// Apply 40 steps for part 2.
const BREAK_POINT_2: u8 = 40;

define_error!(
    ParsePolymerError,
    "the manual must have a blank line separating the template and the rules"
);

define_error!(ParseRuleError, r#"a rule must be delimited by " -> ""#);

define_error!(
    ParseTemplateError,
    "a template must contain at least one pair"
);

/// A pair of elements.
#[derive(Debug, Eq, Hash, PartialEq)]
struct Pair {
    /// The left element in the pair.
    left: u8,

    /// The right element in the pair.
    right: u8,
}

impl TryFrom<&[u8]> for Pair {
    type Error = TryFromSliceError;

    fn try_from(slice: &[u8]) -> Result<Self, Self::Error> {
        <[u8; 2]>::try_from(slice).map(|[left, right]| Pair { left, right })
    }
}

/// A pair insertion rule.
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Debug)]
struct Rule {
    pair: Pair,
    insert: u8,
}

impl From<Rule> for (Pair, u8) {
    fn from(Rule { pair, insert }: Rule) -> Self {
        (pair, insert)
    }
}

impl FromStr for Rule {
    type Err = anyhow::Error;

    fn from_str(rule: &str) -> Result<Self, Self::Err> {
        let (pair, insert) = rule.split_once(" -> ").ok_or(ParseRuleError)?;

        let pair = pair.as_bytes().try_into()?;
        let [insert]: [_; 1] = insert.as_bytes().try_into()?;

        Ok(Self { pair, insert })
    }
}

/// The current state of the polymer.
#[derive(Debug, Default)]
struct Polymer {
    /// The element pairs and their frequencies at this state.
    fq: HashMap<Pair, u64>,

    /// The map of pair insertion rules.
    rules: HashMap<Pair, u8>,
}

impl Polymer {
    /// Updates a pair's frequency in the polymer.
    fn insert(&mut self, left: u8, right: u8, frequency: u64) {
        *self.fq.entry(Pair { left, right }).or_default() += frequency;
    }

    /// Applies one step of pair insertion.
    fn step(&mut self) {
        let fq = mem::take(&mut self.fq);

        fq.into_iter().filter(|(_, f)| f != &0).for_each(|(p, f)| {
            let insert = self.rules[&p];

            self.insert(p.left, insert, f);
            self.insert(insert, p.right, f);
        });
    }
}

impl TryFrom<(&[u8], &str)> for Polymer {
    type Error = anyhow::Error;

    fn try_from((template, rules): (&[u8], &str)) -> Result<Self, Self::Error> {
        if template.len() < 2 {
            return Err(ParseTemplateError.into());
        }

        let mut fq = HashMap::new();
        template.windows(2).try_for_each(|pair| {
            pair.try_into()
                .map(|pair| *fq.entry(pair).or_default() += 1)
        })?;

        let rules = rules
            .lines()
            .map(|line| line.parse::<Rule>().map(Into::into))
            .collect::<Result<_, _>>()?;

        Ok(Self { fq, rules })
    }
}

/// A counter for the quantities of elements in the polymer.
#[derive(Debug, Default)]
struct Count {
    /// The leftmost element in the polymer.
    leftmost: u8,

    /// The polymer.
    polymer: Polymer,

    /// Each element in the polymer and its quantity.
    count: HashMap<u8, u64>,
}

impl Count {
    /// Prepares the counter.
    fn reset(&mut self) {
        self.count.clear();

        // The leftmost pair (and the rightmost one) do not overlap other pairs. This means that
        // the leftmost element in the polymer does not have a right counterpart.
        // When counting the quantities of the elements, only the right sides of the element pairs
        // are counted, so the leftmost element must be accounted for separately.
        self.count.insert(self.leftmost, 1);
    }

    /// Returns the difference between quantity of the most common element and that of least common
    /// one.
    fn range(&mut self) -> u64 {
        self.reset();

        self.polymer
            .fq
            .iter()
            .filter_map(|(p, &f)| if f == 0 { None } else { Some((p.right, f)) })
            .for_each(|(p, f)| *self.count.entry(p).or_default() += f);

        let mut count = self.count.values();

        let mut least = &0;
        let mut most = &0;

        if let Some(next) = count.next() {
            least = next;
            most = next;

            count.for_each(|count| {
                least = least.min(count);
                most = most.max(count);
            });
        }

        most - least
    }
}

impl FromStr for Count {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (template, rules) = input
            .split_once(input.double_line_ending())
            .ok_or(ParsePolymerError)?;

        let template = template.as_bytes();

        let polymer = (template, rules).try_into()?;
        let leftmost = template[0];

        Ok(Self {
            leftmost,
            polymer,
            ..Default::default()
        })
    }
}

/// This puzzle.
struct D14;

impl Puzzle for D14 {
    type Solution = [u64; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut count = input.parse::<Count>()?;

        (0..BREAK_POINT_1).for_each(|_| count.polymer.step());
        let p_1 = count.range();

        (BREAK_POINT_1..BREAK_POINT_2).for_each(|_| count.polymer.step());
        let p_2 = count.range();

        Ok([p_1, p_2])
    }
}

aoc_2021::main!(D14);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample() {
        D14.check("../Inputs/D14/sample.txt", [1588, 2_188_189_693_529]);
    }

    #[test]
    fn input() {
        D14.check("../Inputs/D14/input.txt", [4244, 4_807_056_953_866]);
    }
}
