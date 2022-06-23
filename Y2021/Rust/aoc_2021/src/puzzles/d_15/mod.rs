#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 15.](https://adventofcode.com/2021/day/15)

use std::{cmp::Ordering, str::FromStr};

use crate::{
    define_error,
    errors::ParseDigitError,
    extension_traits::u8::U8Ext,
    integer::div_rem::DivRem,
    min_heap::MinHeap,
    pair::{greater::Greater, Pair},
    Puzzle,
};

define_error!(
    Unreachable,
    "there is no possible path from the top left to the bottom right of the map"
);

/// The minimum risk level.
const MIN: u16 = 1;

/// The maximum risk level.
const MAX: u16 = 9;

/// The factor to scale the tile by for part 2.
const FACTOR: u16 = 5;

/// The position on the map.
type Position = Pair<u16>;

/// The risk level of a node.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Risk {
    /// The node is unreachable.
    Unreachable,

    /// The node is reachable has this risk level.
    Some(u16),
}

impl Default for Risk {
    fn default() -> Self {
        Self::Some(Default::default())
    }
}

impl Ord for Risk {
    // Note: this does not return Ordering::Equal if both self and other are Self::Unreachable;
    // this is fine because the ordering does not matter if both nodes are unreachable.
    // This is done to simplify the pattern matching, and it is why Ord is manually implemented
    // instead of derived.
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Unreachable, _) => Ordering::Greater,
            (_, Self::Unreachable) => Ordering::Less,
            (Self::Some(l), Self::Some(r)) => l.cmp(r),
        }
    }
}

impl PartialOrd for Risk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Risk> for Result<u16, Unreachable> {
    fn from(risk: Risk) -> Self {
        match risk {
            Risk::Some(r) => Ok(r),
            Risk::Unreachable => Err(Unreachable),
        }
    }
}

/// A node in the graph.
#[derive(Debug, Default, Eq, PartialEq)]
struct Node {
    /// The node's position.
    p: Position,

    /// The node's risk level.
    risk: Risk,
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        self.risk.cmp(&other.risk)
    }
}

impl From<Position> for Node {
    fn from(p: Position) -> Self {
        Node {
            p,
            ..Default::default()
        }
    }
}

/// The risk level map.
#[derive(Debug, Default)]
struct Graph {
    /// A matrix of the risk levels.
    nodes: Vec<Vec<u16>>,

    /// The size of a tile in the map.
    size: u16,

    /// The size of the entire map.
    scaled: u16,

    /// A heap that is used to extract the lowest-risk neighbour.
    heap: MinHeap<Node>,
}

impl Graph {
    /// Returns the risk level of a position.
    fn get(&self, Pair(r, c): Position) -> u16 {
        let r = DivRem::new(r, self.size);
        let c = DivRem::new(c, self.size);

        let risk = self.nodes[usize::from(r.rem)][usize::from(c.rem)];

        (risk + r.div + c.div - MIN) % MAX + MIN
    }

    /// Returns the lowest total risk of any path from the top left to the bottom right of the map.
    //
    // Uses Dijkstra's algorithm.
    fn min_risk(&mut self) -> Risk {
        let length = usize::from(self.scaled);
        let mut neighbours = vec![vec![Risk::Unreachable; length]; length];

        neighbours[0][0] = Risk::Some(0);
        self.heap.push(Node::default());

        while let Some(Node { p, risk }) = self.heap.pop() {
            if let Risk::Some(risk) = risk {
                for neighbour @ Pair(r, c) in p
                    .adjacent_cardinal()
                    .filter(|p| Pair(self.scaled, self.scaled).gt(p))
                {
                    let next = Node {
                        p: neighbour,
                        risk: Risk::Some(self.get(neighbour) + risk),
                    };

                    let risk = &mut neighbours[usize::from(r)][usize::from(c)];

                    if next.risk < *risk {
                        *risk = next.risk;
                        self.heap.push(next);
                    }
                }
            }
        }

        let last = length - 1;
        neighbours[last][last]
    }
}

impl FromIterator<Vec<u16>> for Graph {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Vec<u16>>,
    {
        let nodes = iter.into_iter().collect::<Vec<_>>();

        let size = nodes.len();

        debug_assert!(size > 0);
        debug_assert_eq!(size, nodes[0].len());

        // CAST: it is okay to cast if the assertion succeeds.
        debug_assert!(size < usize::from(u16::MAX));
        let size = size as u16;

        Self {
            nodes,
            size,
            scaled: size,
            ..Default::default()
        }
    }
}

impl FromStr for Graph {
    type Err = ParseDigitError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(|line| {
                line.bytes()
                    .map(|byte| byte.parse_digit().map(u16::from))
                    .collect()
            })
            .collect()
    }
}

/// This puzzle.
pub struct D15;

impl Puzzle for D15 {
    type Solution = [u16; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut graph = input.parse::<Graph>()?;

        let p_1 = Result::from(graph.min_risk())?;

        graph.scaled *= FACTOR;
        let p_2 = Result::from(graph.min_risk())?;

        Ok([p_1, p_2])
    }
}
