use aoc_2021::puzzles::d_02::D02;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D02.check("D02/sample.txt", [150, 900]);
}

#[test]
fn input() {
    D02.check("D02/input.txt", [1_714_680, 1_963_088_820]);
}
