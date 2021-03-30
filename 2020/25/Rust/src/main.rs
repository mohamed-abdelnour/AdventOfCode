mod modular;
mod naive;

use modular as M;
use naive as N;

use std::env;
use std::fs;

const MODULUS: u128 = 20_201_227;

fn parse_input(input: &str) -> (u128, u128) {
    let input = input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect::<Vec<_>>();
    (input[0], input[1])
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Naive:   {}", N::solve(&parsed));
    println!("  Modular: {}", M::solve(&parsed).unwrap());
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
