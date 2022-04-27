use aoc_2021::puzzles::d_09::D09;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D09.check("../../Inputs/D09/sample.txt", [15, 1134]);
}

#[test]
fn input() {
    D09.check("../../Inputs/D09/input.txt", [512, 1_600_104]);
}
