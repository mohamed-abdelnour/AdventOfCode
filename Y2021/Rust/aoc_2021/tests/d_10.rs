use aoc_2021::puzzles::d_10::D10;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D10.check("../../Inputs/D10/sample.txt", [26_397, 288_957]);
}

#[test]
fn input() {
    D10.check("../../Inputs/D10/input.txt", [364_389, 2_870_201_088]);
}
