mod part_1;
mod part_2;

use part_1 as P1;
use part_2 as P2;

use std::convert::TryFrom;
use std::env;
use std::fs;

fn parse_input(input: &str) -> Vec<i32> {
    let u32_to_i32 = |x: &u32| i32::try_from(*x).unwrap();
    input
        .lines()
        .take(1)
        .collect::<String>()
        .chars()
        .map(|x| u32_to_i32(&x.to_digit(10).unwrap()))
        .collect()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", P1::solve(&parsed));
    let (r_1, r_2, p) = P2::solve(&parsed);
    println!("  Part 2: {} * {} = {}", r_1, r_2, p);
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
