use aoc_2021::puzzles::d_15::D15;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D15.check("D15/sample.txt", [40, 315]);
}

#[test]
fn input() {
    D15.check("D15/input.txt", [581, 2916]);
}
