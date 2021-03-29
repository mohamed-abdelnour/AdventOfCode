use std::env;
use std::fs;

fn parse_input(input: &str) -> Vec<Vec<char>> {
    let input = input.lines().collect::<Vec<_>>();
    input
        .iter()
        .filter(|x| !x.is_empty())
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>()
}

fn decode(character: char, bounds: (u32, u32)) -> (u32, u32) {
    let (mut lower, mut upper) = bounds;
    let next = (lower + upper) / 2;
    if (character == 'F') || (character == 'L') {
        upper = next;
    } else {
        lower = next + 1;
    }
    (lower, upper)
}

fn get_id(input: &[char]) -> u32 {
    let mut rows = (0, 127);
    let mut columns = (0, 7);
    for item in input.iter() {
        if rows.0 != rows.1 {
            rows = decode(*item, rows);
        } else {
            columns = decode(*item, columns);
        }
    }
    8 * rows.0 + columns.0
}

fn first_different(input: &[(u32, u32)]) -> Option<u32> {
    for (reference, value) in input {
        if reference != value {
            return Some(*reference);
        }
    }
    None
}

fn part_1(input: &[Vec<char>]) -> Vec<u32> {
    input.iter().map(|x| get_id(x)).collect()
}

fn part_2(input: &[Vec<char>]) -> u32 {
    let mut input = part_1(input);
    input.sort_unstable();
    let input = (input[0]..).zip(input).collect::<Vec<_>>();
    first_different(&input).unwrap()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);
    let p1 = part_1(&parsed);
    let p1 = p1.iter().max().unwrap();

    println!("File: {}", path);
    println!("  Part 1: {}", p1);
    println!("  Part 2: {}", part_2(&parsed));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
