use std::env;
use std::fs;

#[derive(Debug)]
struct Note<'a> {
    time: i64,
    id_list: Vec<&'a str>,
}

impl<'a> Note<'a> {
    fn new(t: i64, ids: Vec<&str>) -> Note {
        Note {
            time: t,
            id_list: ids,
        }
    }
}

struct ParsedNote {
    time: i64,
    id_list: Vec<i64>,
}

fn parse_input(input: &str, remove_x: bool) -> Note {
    let input = input.lines().filter(|x| !x.is_empty()).collect::<Vec<_>>();
    let time = input[0].parse().unwrap();
    let mut id_list = input[1].split(',').collect::<Vec<_>>();
    if remove_x {
        id_list = id_list.iter().cloned().filter(|x| x != &"x").collect();
    }
    Note::new(time, id_list)
}

fn init_input(note: &Note) -> ParsedNote {
    ParsedNote {
        time: note.time,
        id_list: note.id_list.iter().map(|x| x.parse().unwrap()).collect(),
    }
}

fn closest_bus(note: &ParsedNote) -> i64 {
    let time = note.time;
    let id_list = &note.id_list;
    for t in time.. {
        for id in id_list {
            let rem = t % id;
            if rem == 0 {
                return (t - time) * id;
            }
        }
    }
    0
}

fn prep_times(id_list: &[&str]) -> (Vec<(i64, i64)>, i64) {
    let mut times = Vec::new();
    let mut product = 1;
    for (offset, id) in id_list.iter().enumerate() {
        if id != &"x" {
            let id = id.parse().unwrap();
            times.push((id - offset as i64, id));
            product *= id;
        }
    }
    (times, product)
}

fn separate_tuples<T: Copy, U: Copy>(list: &[(T, U)]) -> (Vec<T>, Vec<U>) {
    let mut left = Vec::new();
    let mut right = Vec::new();
    for element in list {
        let (l, r) = element;
        left.push(*l);
        right.push(*r);
    }
    (left, right)
}

fn get_divs(product: i64, times: &[i64]) -> (Vec<i64>, Vec<i64>) {
    let mut div_selves = Vec::new();
    let mut divs = Vec::new();
    for time in times {
        let div_self = product / time;
        let div = div_self % time;
        div_selves.push(div_self);
        divs.push(div);
    }
    (div_selves, divs)
}

fn find_mod(a: &i64, b: &i64) -> i64 {
    for n in 1.. {
        if (a * n) % b == 1 {
            return n;
        }
    }
    0
}

fn find_mods(divs: &[i64], times: &[i64]) -> Vec<i64> {
    let mut result = Vec::new();
    for (d, m) in divs.iter().zip(times.iter()) {
        result.push(find_mod(d, m));
    }
    result
}

fn get_sum(diffs: &[i64], div_selves: &[i64], mods: &[i64]) -> i64 {
    let mut sum = 0;
    for i in 0..diffs.len() {
        sum += diffs[i] * div_selves[i] * mods[i];
    }
    sum
}

fn part_1(input: &str) -> i64 {
    closest_bus(&init_input(&parse_input(&input, true)))
}

fn part_2(input: &str) -> i64 {
    let (pairs, product) = prep_times(&parse_input(&input, false).id_list);
    let (diffs, times) = separate_tuples(&pairs);
    let (div_selves, divs) = get_divs(product, &times);
    let mods = find_mods(&divs, &times);
    let sum = get_sum(&diffs, &div_selves, &mods);
    sum % product
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();

    println!("File: {}", path);
    println!("  Part 1: {}", part_1(&input));
    println!("  Part 2: {}", part_2(&input));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
