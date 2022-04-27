use aoc_2021::puzzles::d_14::D14;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D14.check("../../Inputs/D14/sample.txt", [1588, 2_188_189_693_529]);
}

#[test]
fn input() {
    D14.check("../../Inputs/D14/input.txt", [4244, 4_807_056_953_866]);
}
