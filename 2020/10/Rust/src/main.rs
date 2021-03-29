use std::env;
use std::fs;

fn parse_input(input: &str) -> Vec<u64> {
    let input = input.lines().filter(|x| !x.is_empty()).collect::<Vec<_>>();
    input.iter().map(|x| x.parse().unwrap()).collect()
}

fn init_input(input: Vec<u64>) -> Vec<u64> {
    let maximum = input.iter().max().unwrap();
    let mut input = input.to_owned();
    input.append(&mut vec![0, maximum + 3]);
    input.sort_unstable();
    input
}

fn chain(input: &[u64]) -> Vec<u64> {
    let mut result = Vec::new();
    for i in 0..input.len() - 1 {
        result.push(input[i + 1] - input[i]);
    }
    result
}

fn connections(chain: &[u64]) -> Vec<usize> {
    let mut result = Vec::new();
    for (index, link) in chain.iter().enumerate() {
        let connection = chain[index + 1..]
            .iter()
            .take_while(|&x| x <= &(link + 3))
            .count();
        result.push(connection);
    }
    result
}

fn combinations(connections: &[usize]) -> Vec<u64> {
    if connections.is_empty() {
        return vec![];
    } else if connections == vec![0] {
        return vec![1];
    }
    let mut connections = connections.to_owned();
    let connection = connections.remove(0);
    let mut next = combinations(&connections);
    let first: u64 = next.iter().take(connection).sum();
    next.insert(0, first);
    next
}

fn part_1(input: &[u64]) -> u64 {
    let chained = chain(input);
    let mut one = 0;
    let mut three = 0;
    for link in chained {
        if link == 1 {
            one += 1;
        } else {
            three += 1;
        }
    }
    one * three
}

fn part_2(input: &[u64]) -> u64 {
    combinations(&connections(input))[0]
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let init = init_input(parse_input(&input));

    println!("File: {}", path);
    println!("  Part 1: {}", part_1(&init));
    println!("  Part 2: {}", part_2(&init));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
