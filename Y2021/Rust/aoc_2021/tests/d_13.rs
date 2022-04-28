use aoc_2021::puzzles::d_13::{Position, Solution, D13, DOT, UNMARKED};

mod util;
use util::PuzzleExt;

struct PositionWrapper(Position);

impl From<char> for PositionWrapper {
    fn from(c: char) -> Self {
        match c {
            DOT => Self(Position::Dot),
            UNMARKED => Self(Position::Unmarked),
            _ => unreachable!(),
        }
    }
}

fn grid(s: &str) -> Vec<Vec<Position>> {
    s.lines()
        .skip(1)
        .map(|l| l.chars().map(|c| PositionWrapper::from(c).0).collect())
        .collect()
}

#[test]
fn sample() {
    const CODE: &str = "
█████
█   █
█   █
█   █
█████";

    let overlaps = 17;
    let code = grid(CODE);
    D13.check("D13/sample.txt", Solution { overlaps, code });
}

#[test]
fn input() {
    const CODE: &str = "
 ██  ███  ████ █    ███  ████ ████ █
█  █ █  █    █ █    █  █ █       █ █
█    █  █   █  █    █  █ ███    █  █
█    ███   █   █    ███  █     █   █
█  █ █    █    █    █    █    █    █
 ██  █    ████ ████ █    █    ████ ████";

    let overlaps = 607;
    let code = grid(CODE);
    D13.check("D13/input.txt", Solution { overlaps, code });
}
