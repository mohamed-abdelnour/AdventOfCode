use std::env;
use std::fs;

#[derive(Debug)]
struct Password {
    range: Vec<usize>,
    character: char,
    password: String,
}

impl Password {
    fn new(line: &[&str]) -> Password {
        Password {
            range: get_range(line[0]),
            character: get_char(line[1]),
            password: line[2].to_string(),
        }
    }
}

fn valid_1(pass: Password) -> bool {
    let count = pass
        .password
        .chars()
        .filter(|x| x == &pass.character)
        .count();
    count >= pass.range[0] && count <= pass.range[1]
}

fn valid_2(pass: Password) -> bool {
    let offset_range = pass.range.iter().map(|x| x - 1).collect::<Vec<_>>();
    let p1 = offset_range[0];
    let p2 = offset_range[1];
    (pass.character == pass.password.chars().nth(p1).unwrap())
        != (pass.character == pass.password.chars().nth(p2).unwrap())
}

fn get_range(range: &str) -> Vec<usize> {
    range.split('-').map(|x| x.parse().unwrap()).collect()
}

fn get_char(character: &str) -> char {
    character.strip_suffix(":").unwrap().chars().next().unwrap()
}

fn parse_input(input: &str) -> Vec<Vec<&str>> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| x.split_whitespace().collect::<Vec<_>>())
        .collect()
}

fn solve<F>(parsed: &[Vec<&str>], func: F) -> usize
where
    F: Fn(Password) -> bool,
{
    parsed
        .iter()
        .map(|x| func(Password::new(x)))
        .filter(|x| *x)
        .count()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", solve(&parsed, valid_1));
    println!("  Part 2: {}", solve(&parsed, valid_2));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
