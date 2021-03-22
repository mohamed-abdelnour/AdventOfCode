use std::env;
use std::fs;

fn find_trees(input: &[String], slope: (usize, usize)) -> usize {
    let (dx, dy) = slope;
    let mut count = 0;
    let mut n = 0;
    for (index, value) in input.iter().enumerate() {
        if index % dx == 0 && !value.is_empty() {
            let p = (n * dy) % value.len();
            if value.chars().nth(p).unwrap() == '#' {
                count += 1;
            }
            n += 1;
        }
    }
    count
}

fn parse_input(input: String) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(input);
    let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)];
    let part_2: usize =
        slopes.iter().map(|x| find_trees(&parsed, *x)).product();

    println!("File: {}", path);
    println!("  Part 1: {}", find_trees(&parsed, (1, 3)));
    println!("  Part 2: {}", part_2);
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
