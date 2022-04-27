use aoc_2021::puzzles::d_04::D04;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D04.check("../../Inputs/D04/sample.txt", [4512, 1924]);
}

#[test]
fn input() {
    D04.check("../../Inputs/D04/input.txt", [54_275, 13_158]);
}
