use std::collections::HashSet;
use std::env;
use std::fs;

fn parse_input(input: &str) -> Vec<Vec<&str>> {
    let mut vector = Vec::new();
    let mut result = Vec::new();
    for line in input.lines() {
        if line.is_empty() {
            vector.push(result);
            result = vec![];
        } else {
            result.push(line);
        }
    }
    if !result.is_empty() {
        vector.push(result)
    };
    vector
}

fn part_1(input: &[Vec<&str>]) -> usize {
    let mut input = input
        .iter()
        .map(|x| x.concat().chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    input.iter_mut().for_each(|x| x.sort_unstable());
    input.iter_mut().for_each(|x| x.dedup());
    input.iter().map(|x| x.len()).sum()
}

fn intersect(input: &[&str]) -> usize {
    let first = input[0];
    let mut set = first.chars().collect::<HashSet<_>>();
    for item in input.iter().skip(1) {
        let next_set = item.chars().collect::<HashSet<_>>();
        set = set.intersection(&next_set).cloned().collect::<HashSet<_>>();
    }
    set.len()
}

fn part_2(input: &[Vec<&str>]) -> usize {
    input.iter().map(|x| intersect(x)).sum()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1(&parsed));
    println!("  Part 2: {}", part_2(&parsed));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
