use std::collections::HashSet;
use std::env;
use std::fs;

fn parse_input(input: String) -> Vec<i32> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect()
}

fn two_sum_n(n: i32, xs: &[i32]) -> Option<Vec<i32>> {
    let mut pairs = HashSet::new();
    for x in xs {
        let y = n - x;
        if pairs.contains(&y) {
            return Some(vec![*x, y]);
        } else {
            pairs.insert(x);
        }
    }
    None
}

fn three_sum_n(n: i32, xs: &[i32]) -> Option<Vec<i32>> {
    let mut ys = xs.to_owned();
    for x in xs {
        ys.remove(0);
        if let Some(mut result) = two_sum_n(n - x, &ys) {
            result.push(*x);
            return Some(result);
        }
    }
    None
}

fn output(path: &str) {
    let parsed = parse_input(fs::read_to_string(&path).unwrap());
    let prod = |vector: Vec<i32>| -> i32 { vector.iter().product() };
    let part_1 = prod(two_sum_n(2020, &parsed).unwrap());
    let part_2 = prod(three_sum_n(2020, &parsed).unwrap());

    println!("File: {}", path);
    println!("  Part 1: {}", part_1);
    println!("  Part 2: {}", part_2);
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
