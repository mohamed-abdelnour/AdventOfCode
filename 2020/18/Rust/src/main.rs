use std::env;
use std::fs;

fn precedence(mode: u64, input: &str) -> u64 {
    if input == "+" {
        return mode;
    } else if input == "*" {
        return 1;
    }
    0
}

fn is_operator(input: &str) -> bool {
    let operators = vec!["+", "*"];
    if operators.contains(&input) {
        return true;
    }
    false
}

fn is_left_paren(input: &str) -> bool {
    if input == "(" {
        return true;
    }
    false
}

fn is_right_paren(input: &str) -> bool {
    if input == ")" {
        return true;
    }
    false
}

fn is_digit(input: &char) -> bool {
    if ('0'..='9').contains(input) {
        return true;
    }
    false
}

fn is_number(input: &str) -> bool {
    input.chars().all(|x| is_digit(&x))
}

fn is_special(input: &str) -> bool {
    let special = |x| is_operator(x) || is_left_paren(x) || is_right_paren(x);
    if special(input) {
        return true;
    }
    false
}

fn parse_line(line: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut characters = Vec::new();
    let append = |characters: Vec<char>, result: &[String]| {
        let mut result = result.to_vec();
        if !characters.is_empty() {
            let number = characters.iter().collect::<String>();
            result.push(number);
        }
        result
    };
    for character in line.chars() {
        let string = character.to_string();
        if is_digit(&character) {
            characters.push(character);
        } else if is_special(&string) {
            result = append(characters, &result);
            result.push(string);
            characters = Vec::new();
        }
    }
    result = append(characters, &result);
    result
}

fn parse_input(input: &str) -> Vec<Vec<String>> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parse_line(x))
        .collect()
}

fn shunting_yard_by<F>(tokens: &[String], precedence: F) -> Vec<&String>
where
    F: Fn(&str) -> u64,
{
    let mut operators = Vec::new();
    let mut result = Vec::new();
    let mut tokens = tokens.iter().rev().collect::<Vec<_>>();
    while !tokens.is_empty() {
        let token = tokens.pop().unwrap();
        if is_number(token) {
            result.push(token);
        } else if is_operator(token) {
            if operators.is_empty() {
                operators.push(token);
            } else {
                let operator = operators.pop().unwrap();
                if precedence(token) <= precedence(operator) {
                    result.push(operator);
                    tokens.push(token);
                } else {
                    operators.push(operator);
                    operators.push(token);
                }
            }
        } else if is_left_paren(token) {
            operators.push(token);
        } else if is_right_paren(token) {
            let last_operator = operators.pop().unwrap();
            if last_operator != "(" {
                result.push(last_operator);
                tokens.push(token);
            }
        }
    }
    for operator in operators.iter().rev() {
        result.push(operator);
    }
    result
}

fn rpn(tokens: &[&String]) -> u64 {
    let eval = |mut acc: Vec<u64>, x: &String| {
        if (acc.len() > 1) && is_operator(x) {
            let a = acc.pop().unwrap();
            let b = acc.pop().unwrap();
            if x == "+" {
                acc.push(a + b);
            } else if x == "*" {
                acc.push(a * b);
            }
        } else {
            acc.push(x.parse().unwrap())
        }
        acc
    };
    let result = tokens.iter().fold(vec![], |acc, x| eval(acc, x));
    result[0]
}

fn solve(mode: u64, input: &[Vec<String>]) -> u64 {
    input
        .iter()
        .map(|x| rpn(&shunting_yard_by(x, |t| precedence(mode, t))))
        .sum()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", solve(1, &parsed));
    println!("  Part 2: {}", solve(2, &parsed));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
