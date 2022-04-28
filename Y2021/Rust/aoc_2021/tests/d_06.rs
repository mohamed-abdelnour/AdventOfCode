use aoc_2021::puzzles::d_06::D06;

mod util;
use util::PuzzleExt;

#[test]
fn sample() {
    D06.check("D06/sample.txt", [5934, 26_984_457_539]);
}

#[test]
fn input() {
    D06.check("D06/input.txt", [365_862, 1_653_250_886_439]);
}
