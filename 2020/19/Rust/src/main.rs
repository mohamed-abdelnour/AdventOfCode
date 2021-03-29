use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::iter;

type Rule = String;
type RuleSet = HashMap<i32, Rule>;

fn parse_input(input: &str) -> Vec<Vec<&str>> {
    input.split("\n\n").map(|x| x.lines().collect()).collect()
}

fn init_input<'a>(input: &[Vec<&'a str>]) -> (RuleSet, Vec<&'a str>) {
    let rules = &input[0];
    let messages = input[1].to_vec();
    let mut rule_set = HashMap::new();

    rules.iter().for_each(|x| {
        let x = x.split(':').collect::<Vec<_>>();
        let id = x[0].parse::<i32>().unwrap();
        let matches = x[1]
            .chars()
            .skip(1)
            .filter(|x| x != &'"')
            .collect::<String>();
        rule_set.insert(id, matches);
    });

    (rule_set, messages)
}

fn is_independent(input: &str) -> bool {
    let special = vec!['|', '(', ')', '{', '}', '+'];
    input
        .chars()
        .all(|x| x.is_ascii_alphabetic() || special.contains(&x))
}

fn parse_rule(rule_set: &RuleSet, rules: &[&str]) -> String {
    let mut result = Vec::new();
    for rule in rules {
        if is_independent(rule) {
            result.push(rule.to_string());
        } else {
            let next = rule_set.get(&rule.parse::<i32>().unwrap()).unwrap();
            if is_independent(next) {
                result.push(next.to_string());
            } else {
                let mut padded = "( ".to_string();
                padded.push_str(next);
                padded.push_str(" )");
                result.push(padded);
            }
        }
    }
    result.concat()
}

fn resolve_rule(rule_set: &RuleSet) -> String {
    let rule = rule_set.get(&0).unwrap();
    let mut parsed = rule.to_owned();
    while !is_independent(&parsed) {
        let rule = parsed.split_whitespace().collect::<Vec<_>>();
        parsed = parse_rule(rule_set, &rule);
    }
    parsed.push('$');
    parsed.insert(0, '^');
    parsed
}

fn match_regex(regex: &Regex, messages: &[&str]) -> usize {
    messages.iter().filter(|x| regex.is_match(x)).count()
}

fn set_quantifier(regex: &str, num: usize) -> String {
    let mut result = String::new();
    for character in regex.chars().skip(1) {
        if character == '}' {
            result.push_str(&num.to_string());
        }
        result.push_str(&character.to_string());
    }
    result.insert(0, '^');
    result
}

fn get_base_regex(regex: &str) -> Vec<String> {
    let regex = regex.split('+').collect::<Vec<_>>();

    let mut first = regex[0].to_owned();
    first.push_str(")$");

    let mut second = regex[1].to_owned();
    second = set_quantifier(&second, 1);

    vec![first, second]
}

fn min_regex_length(regex: &str) -> usize {
    let wildcard = regex
        .chars()
        .map(|x| if vec!['a', 'b'].contains(&x) { '.' } else { x })
        .collect::<String>();
    let wildcard = Regex::new(&wildcard).unwrap();
    for n in 1.. {
        let control = iter::repeat('x').take(n).collect::<String>();
        if wildcard.is_match(&control) {
            return n;
        }
    }
    0
}

fn regex_in_range(
    first: usize,
    second: usize,
    longest: usize,
    regex: &str,
) -> Vec<Regex> {
    let mut result = Vec::new();
    for n in 1.. {
        if first + second * n <= longest {
            let current = Regex::new(&set_quantifier(&regex, n)).unwrap();
            result.push(current);
        } else {
            return result;
        }
    }
    vec![]
}

fn part_1(rule_set: &RuleSet, messages: &[&str]) -> usize {
    let regex = Regex::new(&resolve_rule(rule_set)).unwrap();
    match_regex(&regex, messages)
}

fn part_2(rule_set: &RuleSet, messages: &[&str]) -> usize {
    let mut rule_set = rule_set.to_owned();
    rule_set.insert(8, "42 +".to_string());
    rule_set.insert(11, "42 {} 31 {}".to_string());

    let longest = messages
        .iter()
        .max_by(|x, y| x.len().cmp(&y.len()))
        .unwrap()
        .len();

    let regex = resolve_rule(&rule_set);
    let base_regex = get_base_regex(&regex);
    let range = base_regex
        .iter()
        .map(|x| min_regex_length(x))
        .collect::<Vec<_>>();
    let first = range[0];
    let second = range[1];
    let results = regex_in_range(first, second, longest, &regex);
    results.iter().map(|x| match_regex(x, messages)).sum()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);
    let (rule_set, messages) = init_input(&parsed);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1(&rule_set, &messages));
    println!("  Part 2: {}", part_2(&rule_set, &messages));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
