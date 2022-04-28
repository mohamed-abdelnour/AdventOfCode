use aoc_2021::puzzles::d_07::D07;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D07.check("D07/sample.txt", [37, 168]);
}

#[test]
fn input() {
    D07.check("D07/input.txt", [341_534, 93_397_632]);
}
