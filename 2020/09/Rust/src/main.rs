use std::cmp::Ordering;
use std::env;
use std::fs;

const CHUNK_SIZE: usize = 25;

fn parse_input(input: &str) -> Vec<i64> {
    let input = input.lines().filter(|x| !x.is_empty()).collect::<Vec<_>>();
    input.iter().map(|x| x.parse().unwrap()).collect()
}

fn sum_two(preamble: &[&i64], number: &i64) -> bool {
    let mut preamble = preamble.to_owned();
    while !preamble.is_empty() {
        let item = preamble.remove(0);
        if preamble.contains(&&(number - item)) {
            return true;
        }
    }
    false
}

fn find_erroneous(chunk_size: &usize, numbers: &[i64]) -> i64 {
    let mut numbers = numbers.to_owned();
    while !numbers.is_empty() {
        let mut preamble = numbers.iter().take(*chunk_size).collect::<Vec<_>>();
        preamble.sort_unstable();
        let input = numbers[*chunk_size];
        if sum_two(&preamble, &input) {
            numbers.remove(0);
        } else {
            return input;
        }
    }
    0
}

fn contiguous_sum(erroneous: &i64, numbers: &[i64]) -> Option<usize> {
    let mut erroneous = erroneous.to_owned();
    for (index, value) in numbers.iter().enumerate() {
        let remainder = erroneous - value;
        match remainder.cmp(&0) {
            Ordering::Less => return None,
            Ordering::Equal => return Some(index),
            Ordering::Greater => erroneous = remainder,
        }
    }
    None
}

fn contiguous_sums(erroneous: &i64, numbers: &[i64]) -> Vec<usize> {
    let mut result = Vec::new();
    let mut clone = numbers.to_owned();
    for (index, _) in numbers.iter().enumerate() {
        let contiguous = contiguous_sum(erroneous, &clone);
        if let Some(cont) = contiguous {
            result.push(vec![index, cont + index]);
        }
        clone.remove(0);
    }
    result
        .iter()
        .max_by(|x, y| (x[1] - x[0]).cmp(&(y[1] - y[0])))
        .unwrap()
        .to_vec()
}

fn part_2(chunk_size: &usize, numbers: &[i64]) -> i64 {
    let erroneous = find_erroneous(chunk_size, numbers);
    let range = contiguous_sums(&erroneous, numbers);
    let lower = range[0];
    let upper = range[1];
    let mut in_range = numbers
        .iter()
        .take(upper + 1)
        .skip(lower)
        .collect::<Vec<_>>();
    in_range.sort_unstable();
    in_range[0] + *in_range.last().unwrap()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", find_erroneous(&CHUNK_SIZE, &parsed));
    println!("  Part 2: {}", part_2(&CHUNK_SIZE, &parsed));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
