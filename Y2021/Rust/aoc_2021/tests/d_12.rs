use aoc_2021::puzzles::d_12::D12;

mod util;
use util::PuzzleExt;

#[test]
fn sample_1() {
    D12.check("../../Inputs/D12/sample_1.txt", [10, 36]);
}

#[test]
fn sample_2() {
    D12.check("../../Inputs/D12/sample_2.txt", [19, 103]);
}

#[test]
fn sample_3() {
    D12.check("../../Inputs/D12/sample_3.txt", [226, 3509]);
}

#[test]
fn input() {
    D12.check("../../Inputs/D12/input.txt", [3410, 98_796]);
}
