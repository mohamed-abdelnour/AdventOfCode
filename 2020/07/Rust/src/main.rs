use std::env;
use std::fs;

fn cruft(word: &str) -> bool {
    let cruft_sample = vec!["contain", "no", "other"];
    if cruft_sample.contains(&word)
        || (word.chars().take(3).collect::<String>() == "bag")
    {
        return true;
    }
    false
}

fn num(word: &str) -> bool {
    for c in word.chars() {
        if !('0'..='9').contains(&c) {
            return false;
        }
    }
    true
}

fn parse_line(line: &str) -> Vec<String> {
    let line = line.split_whitespace().collect::<Vec<_>>();
    let mut parsed = Vec::new();
    for word in line {
        if !cruft(word) {
            parsed.push(word);
        }
    }
    let mut result = Vec::new();
    let mut post_parse = Vec::new();
    for word in parsed {
        if num(word) {
            post_parse.push(result.concat());
            post_parse.push(word.to_string());
            result = vec![];
        } else {
            result.push(word);
        }
    }
    if !result.is_empty() {
        post_parse.push(result.concat());
    }
    post_parse
}

fn parse_input(input: &str) -> Vec<Vec<String>> {
    let input = input.lines().collect::<Vec<_>>();
    input.iter().map(|x| parse_line(x)).collect()
}

fn get_parents(input: &[Vec<String>], child: String) -> Vec<String> {
    let parents = |x: Vec<String>| {
        x.iter()
            .rev()
            .cloned()
            .skip_while(|y| *y != child)
            .skip(1)
            .collect::<Vec<_>>()
    };
    let mut result = Vec::new();
    for line in input {
        let mut current_parents = parents(line.clone());
        if !current_parents.is_empty() {
            result.push(current_parents.pop().unwrap());
        }
    }
    result
}

fn map_parents(
    mut children: Vec<String>,
    input: &[Vec<String>],
) -> Vec<String> {
    let level = |x: &[String]| {
        x.iter()
            .cloned()
            .flat_map(|y| get_parents(input, y))
            .collect::<Vec<_>>()
    };
    let mut result = Vec::new();
    while !children.is_empty() {
        children = level(&children);
        let temp = children.clone();
        result.push(temp);
    }
    result.concat()
}

fn find_line(input: &[Vec<String>], child: &str) -> Vec<String> {
    for line in input {
        if line.iter().next().unwrap() == child {
            return line.clone();
        }
    }
    vec![]
}

fn count_children(input: &[Vec<String>], line: &[String]) -> usize {
    if input.is_empty() && line.is_empty() {
        return 0;
    }
    let quantities = line
        .iter()
        .filter(|x| num(x))
        .map(|y| y.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let children = line.iter().skip(1).filter(|x| !num(x)).collect::<Vec<_>>();
    let next = children
        .iter()
        .map(|x| count_children(input, &find_line(input, x)))
        .collect::<Vec<_>>();
    let count = quantities.iter().zip(next).collect::<Vec<_>>();
    let count = count.iter().fold(0, |acc, (a, b)| acc + *a * b);
    count + quantities.iter().sum::<usize>()
}

fn part_1(input: &[Vec<String>]) -> usize {
    let mut result = map_parents(vec!["shinygold".to_string()], &input);
    result.sort_unstable();
    result.dedup();
    result.len()
}

fn part_2(input: &[Vec<String>]) -> usize {
    let line = find_line(input, "shinygold");
    count_children(input, &line)
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
