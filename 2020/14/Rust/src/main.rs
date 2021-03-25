mod part_1;
mod part_2;

use crate::part_2 as P2;
use crate::part_1 as P1;

use std::env;
use std::fs;

#[derive(Debug)]
pub struct Instruction {
    pub id: String,
    pub value: String,
}

impl Instruction {
    fn new(id: &str, value: &str) -> Instruction {
        Instruction {
            id: id.to_string(),
            value: value.to_string(),
        }
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    for instruction in input.lines() {
        if !instruction.is_empty() {
            let split = instruction.split('=').collect::<Vec<_>>();
            let strip =
                |x: &str| x.chars().filter(|y| y != &' ').collect::<String>();
            let mut id = strip(split[0]);
            if id != "mask" {
                id = id
                    .strip_prefix("mem[")
                    .unwrap()
                    .strip_suffix(']')
                    .unwrap()
                    .to_string();
            }
            let value = strip(split[1]);
            instructions.push(Instruction::new(&id, &value));
        }
    }
    instructions
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", P1::step_memory(&parsed));
    println!("  Part 2: {}", P2::step_input(&parsed));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
