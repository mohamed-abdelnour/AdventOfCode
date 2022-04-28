use aoc_2021::puzzles::d_03::D03;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D03.check("D03/sample.txt", [198, 230]);
}

#[test]
fn input() {
    D03.check("D03/input.txt", [3_320_834, 4_481_199]);
}
