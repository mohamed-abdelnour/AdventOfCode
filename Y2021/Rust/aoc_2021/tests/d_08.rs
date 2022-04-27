use aoc_2021::puzzles::d_08::D08;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D08.check("../../Inputs/D08/sample.txt", [26, 61_229]);
}

#[test]
fn input() {
    D08.check("../../Inputs/D08/input.txt", [416, 1_043_697]);
}
