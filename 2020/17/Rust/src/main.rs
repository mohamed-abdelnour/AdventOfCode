mod part_1;
mod part_2;

use crate::part_1 as P1;
use crate::part_2 as P2;

use std::env;
use std::fs;

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let part_1 = P1::init_grid(&input);
    let part_2 = P2::init_grid(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", P1::cycle_grid(&part_1));
    println!("  Part 2: {}", P2::cycle_grid(&part_2));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
