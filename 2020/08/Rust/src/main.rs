use std::convert::TryFrom;
use std::env;
use std::fs;

fn parse_input(input: &str) -> Vec<&str> {
    input.lines().filter(|x| !x.is_empty()).collect()
}

fn get_instruction(instruction: &str) -> (&str, isize) {
    let instruction = instruction.split_whitespace().collect::<Vec<_>>();
    let argument = instruction[1].parse().unwrap();
    (instruction[0], argument)
}

fn step_machine(instructions: &[&str]) -> (isize, bool) {
    let mut executed = Vec::new();
    let mut index: isize = 0;
    let mut acc: isize = 0;
    let convert_num = |x| usize::try_from(x).unwrap();
    while convert_num(index) != instructions.len() {
        if executed.contains(&index) {
            return (acc, false);
        }
        let instruction = instructions[convert_num(index)];
        let (operation, argument) = get_instruction(instruction);
        executed.push(index);
        if operation == "jmp" {
            index += argument;
        } else if operation == "acc" {
            acc += argument;
            index += 1;
        } else {
            index += 1;
        }
    }
    (acc, true)
}

fn get_fix_indices(instructions: &[&str]) -> Vec<usize> {
    instructions
        .iter()
        .enumerate()
        .filter(|x| {
            let (operation, _) = get_instruction(x.1);
            if ["jmp", "nop"].contains(&operation) {
                return true;
            }
            false
        })
        .map(|x| x.0)
        .collect::<Vec<_>>()
}

fn swap_operation(instructions: &[String], index: usize) -> Vec<String> {
    let this = instructions[index].clone();
    let (operation, argument) = get_instruction(&this);
    let concat_num = |x: &str, y: isize| -> String {
        let mut x = x.to_string();
        x.push_str(&y.to_string());
        x
    };
    let next = {
        if operation == "jmp" {
            concat_num("nop ", argument)
        } else {
            concat_num("jmp ", argument)
        }
    };
    let mut instructions = instructions.to_owned();
    instructions[index] = next;
    instructions.iter().map(|x| x.to_string()).collect()
}

fn find_fixed(set: &[String]) -> isize {
    let mut temp = Vec::new();
    for t in set {
        temp.push(&t[..]);
    }
    let (acc, fixed) = step_machine(&temp);
    if fixed {
        return acc;
    }
    0
}

fn part_2(instructions: &[&str]) -> isize {
    let indices = get_fix_indices(instructions);
    let instructions = instructions
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>();
    for index in indices {
        let acc = find_fixed(&swap_operation(&instructions, index));
        if acc != 0 {
            return acc;
        }
    }
    0
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", step_machine(&parsed).0);
    println!("  Part 2: {}", part_2(&parsed));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
