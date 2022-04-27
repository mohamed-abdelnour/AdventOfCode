use aoc_2021::puzzles::d_05::D05;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D05.check("../../Inputs/D05/sample.txt", [5, 12]);
}

#[test]
fn input() {
    D05.check("../../Inputs/D05/input.txt", [5_608, 20_299]);
}
