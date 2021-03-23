use std::collections::HashSet;
use std::env;
use std::fs;

const MIN_BYR: u32 = 1920;
const MAX_BYR: u32 = 2002;
const MIN_IYR: u32 = 2010;
const MAX_IYR: u32 = 2020;
const MIN_EYR: u32 = 2020;
const MAX_EYR: u32 = 2030;
const MIN_CM: u32 = 150;
const MAX_CM: u32 = 193;
const MIN_IN: u32 = 59;
const MAX_IN: u32 = 76;

fn parse_input(input: &str) -> Vec<Vec<Vec<&str>>> {
    let parsed = input.split("\n\n").collect::<Vec<_>>();
    let parsed = parsed
        .iter()
        .map(|x| x.split_whitespace().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    parsed
        .iter()
        .map(|x| {
            x.iter()
                .map(|y| y.split(':').collect::<Vec<_>>())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn valid_num(num: &str, bounds: (u32, u32)) -> bool {
    let (lower, upper) = bounds;
    let read = num.parse::<u32>().unwrap();
    read >= lower && read <= upper
}

fn valid_byr(byr: &str) -> bool {
    valid_num(byr, (MIN_BYR, MAX_BYR))
}

fn valid_ecl(ecl: &str) -> bool {
    let reference = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        .iter()
        .collect::<HashSet<_>>();
    reference.contains(&ecl)
}

fn valid_eyr(eyr: &str) -> bool {
    valid_num(eyr, (MIN_EYR, MAX_EYR))
}

fn valid_hcl(hcl: &str) -> bool {
    if !hcl.starts_with('#') {
        return false;
    }
    for c in hcl.chars().skip(1) {
        if !(('a'..='f').contains(&c) || ('0'..='9').contains(&c)) {
            return false;
        }
    }
    true
}

fn valid_hgt(hgt: &str) -> bool {
    let hgt = hgt.chars().clone().rev().collect::<Vec<_>>();
    let num = &hgt.iter().skip(2).rev().collect::<String>();
    if (hgt[0] == 'm') && (hgt[1] == 'c') {
        return valid_num(num, (MIN_CM, MAX_CM));
    } else if (hgt[0] == 'n') && (hgt[1] == 'i') {
        return valid_num(num, (MIN_IN, MAX_IN));
    }
    false
}

fn valid_iyr(iyr: &str) -> bool {
    valid_num(iyr, (MIN_IYR, MAX_IYR))
}

fn valid_pid(pid: &str) -> bool {
    pid.len() == 9
}

fn valid_1<'a>(passports: &'a [Vec<Vec<&str>>]) -> Vec<Vec<&'a Vec<&'a str>>> {
    let reference = vec!["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"];
    let mut valid = vec![];
    for passport in passports {
        let mut result = vec![];
        for field in passport {
            let field_id = field[0];
            if field_id != "cid" {
                result.push(field);
            }
        }
        result.sort_unstable();
        let fields = result.iter().map(|x| x[0]).collect::<Vec<_>>();
        if fields == reference {
            valid.push(result);
        }
    }
    valid
}

fn valid_2<'a>(
    passports: &'a [Vec<&Vec<&str>>],
) -> Vec<&'a Vec<&'a Vec<&'a str>>> {
    let mut result = vec![];
    for passport in passports {
        let fields = passport.iter().map(|x| x[1]).collect::<Vec<_>>();
        let valid = valid_byr(fields[0])
            && valid_ecl(fields[1])
            && valid_eyr(fields[2])
            && valid_hcl(fields[3])
            && valid_hgt(fields[4])
            && valid_iyr(fields[5])
            && valid_pid(fields[6]);
        if valid {
            result.push(passport);
        }
    }
    result
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);
    let part_1 = valid_1(&parsed);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1.len());
    println!("  Part 2: {:#?}", valid_2(&part_1).len());
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
