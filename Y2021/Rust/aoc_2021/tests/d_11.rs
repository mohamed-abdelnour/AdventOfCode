use aoc_2021::puzzles::d_11::D11;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D11.check("../../Inputs/D11/sample.txt", [1656, 195]);
}

#[test]
fn input() {
    D11.check("../../Inputs/D11/input.txt", [1627, 329]);
}
