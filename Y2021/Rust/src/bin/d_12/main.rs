#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! [Day 12.](https://adventofcode.com/2021/day/12)

use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::str::FromStr;

use aoc_2021::{define_error, Puzzle};

define_error!(DelimiterError, "connected caves must be seperated by a '-'");

/// Uniquely identifies each cave with an index and keeps track of whether the cave is small or
/// not.
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Cave {
    Small(u8),
    Other(u8),
}

impl From<Cave> for u8 {
    fn from(cave: Cave) -> Self {
        match cave {
            Cave::Small(i) | Cave::Other(i) => i,
        }
    }
}

impl From<Cave> for usize {
    fn from(cave: Cave) -> Self {
        u8::from(cave).into()
    }
}

/// Information used while traversing the graph.
#[derive(Debug, Default)]
struct State {
    /// The small caves visited along a certain path.
    visited: BTreeSet<Cave>,

    /// A cache for `Graph::deep_dfs` results.
    memo: HashMap<u64, u32>,
}

/// The graph of caves.
#[derive(Debug, Default)]
struct Graph {
    /// An adjacency list representation of the graph.
    adjacency: Vec<Vec<Cave>>,

    /// The number of nodes in the graph (excluding `end`).
    length: u8,
}

impl Graph {
    /// Returns the number of paths from `node` to `end` for a given `allowance`.
    fn deep_dfs(&self, node: Cave, allowance: u8, state: &mut State) -> u32 {
        // Hash the function arguments that uniquely identify this call.
        // This avoids having to clone state.visited and allocate a new BTreeSet.
        let mut hasher = DefaultHasher::new();
        (&node, &allowance, &state.visited).hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(&paths) = state.memo.get(&hash) {
            paths
        } else {
            // Keep track of small caves along path.
            if matches!(node, Cave::Small(_)) {
                state.visited.insert(node);
            }

            let mut paths = 0;

            self.adjacency[usize::from(node)]
                .iter()
                .for_each(|adjacent| {
                    // Increment the number of paths if this reaches end.
                    if adjacent == &Cave::Other(self.length) {
                        paths += 1;
                    } else {
                        // Decrement this path's allowance if it already contains this small cave.
                        // This does not change the allowance for other paths.
                        //
                        // With an initial allowance of 2, this means a single small cave can be
                        // visited twice, and all other small caves can only be visited once per
                        // path. The first time a small cave is visited it decrements the allowance
                        // from 2 to 1; thus, all successive calls to deep_dfs on this path will
                        // have a maximum allowance of 1.
                        let allowance = allowance - u8::from(state.visited.contains(adjacent));

                        // Visit adjacent if it is allowed.
                        if allowance != 0 {
                            let length = state.visited.len();

                            paths += self.deep_dfs(*adjacent, allowance, state);

                            // If the recursive call modified state.visited, undo the modification.
                            if state.visited.len() != length {
                                state.visited.remove(adjacent);
                            }
                        }
                    }
                });

            // Memoise the result.
            state.memo.insert(hash, paths);

            paths
        }
    }
}

impl FromStr for Graph {
    type Err = DelimiterError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // Collect all nodes in the graph other than start and end.
        let nodes = input
            .lines()
            .flat_map(|line| line.split('-'))
            .filter(|&node| !matches!(node, "start" | "end"))
            .collect::<HashSet<_>>();

        // Mark small caves and assign an index for each node.
        let mut i = 1;
        let nodes = nodes
            .into_iter()
            .map(|name| {
                let cave = if name.chars().all(|c| c.is_ascii_lowercase()) {
                    Cave::Small(i)
                } else {
                    Cave::Other(i)
                };

                i += 1;

                (name, cave)
            })
            .collect::<HashMap<_, _>>();

        let length = nodes.len() + 1;
        let mut adjacency = vec![Vec::new(); length];

        // CAST: it is okay to cast length to u8 if the assertion succeeds.
        debug_assert!(length <= u8::MAX as usize);
        let length = length as u8;

        // Generate the adjacency list:
        // - start cannot occur in the adjacency list for any node, i.e. no node can go back to
        //   start.
        // - end does not have an adjacency list, i.e. reaching end marks the end of a path; no
        //   other nodes may be visited afterwards.
        // - all other node pairs, (a, b), have two edges associated with them: a -> b and b -> a.
        input.lines().try_for_each(|line| {
            line.split_once('-')
                .ok_or(DelimiterError)
                .map(|(a, b)| match (a, b) {
                    ("start", e) | (e, "start") => adjacency[0].push(nodes[e]),
                    (s, "end") | ("end", s) => {
                        adjacency[usize::from(nodes[s])].push(Cave::Other(length))
                    }
                    _ => {
                        adjacency[usize::from(nodes[a])].push(nodes[b]);
                        adjacency[usize::from(nodes[b])].push(nodes[a]);
                    }
                })
        })?;

        Ok(Self { adjacency, length })
    }
}

/// A wrapper around `Graph` that also holds a `State`. This allows both reusing the state's
/// allocations and taking full advantage of the cache across calls to `Graph::deep_dfs`.
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Debug, Default)]
struct MemoGraph {
    graph: Graph,
    state: State,
}

impl MemoGraph {
    /// Returns the number of paths from `start` to `end` for a given `allowance`.
    //
    // Note that this fixes the start node to `start`; this means that it leaves
    // `self.state.visited` empty. If the starting node were a small cave, `self.state.visited`
    // would have to be cleared after each call.
    fn deep_dfs(&mut self, allowance: u8) -> u32 {
        self.graph
            .deep_dfs(Cave::Other(0), allowance, &mut self.state)
    }
}

impl FromStr for MemoGraph {
    type Err = DelimiterError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input.parse::<Graph>().map(|graph| Self {
            graph,
            ..Default::default()
        })
    }
}

/// This puzzle.
struct D12;

impl Puzzle for D12 {
    type Solution = [u32; 2];

    fn solve(&self, input: String) -> anyhow::Result<Self::Solution> {
        let mut graph = input.parse::<MemoGraph>()?;
        Ok([graph.deep_dfs(1), graph.deep_dfs(2)])
    }
}

aoc_2021::main!(D12);

#[cfg(test)]
mod tests {
    use test_utils::*;

    use super::*;

    #[test]
    fn sample_1() {
        D12.check("../Inputs/D12/sample_1.txt", [10, 36]);
    }

    #[test]
    fn sample_2() {
        D12.check("../Inputs/D12/sample_2.txt", [19, 103]);
    }

    #[test]
    fn sample_3() {
        D12.check("../Inputs/D12/sample_3.txt", [226, 3509]);
    }

    #[test]
    fn input() {
        D12.check("../Inputs/D12/input.txt", [3410, 98_796]);
    }
}
