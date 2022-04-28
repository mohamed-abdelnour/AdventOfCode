use aoc_2021::puzzles::d_01::D01;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D01.check("D01/sample.txt", [7, 5]);
}

#[test]
fn input() {
    D01.check("D01/input.txt", [1665, 1702]);
}
