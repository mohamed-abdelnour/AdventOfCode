use std::char;
use std::convert::TryFrom;
use std::iter;

const ITERATIONS: usize = 100;

fn find_destination(cup: &i32, cups: &[i32]) -> i32 {
    let mut cup = cup.to_owned();
    let min = cups.iter().min().unwrap();
    let max = cups.iter().max().unwrap();
    let destinations = iter::repeat_with(|| {
        let next = cup;
        cup -= 1;
        next
    })
    .take_while(|x| x >= min)
    .collect::<Vec<_>>();
    let candidates = destinations
        .iter()
        .map(|x| (x, cups.contains(x)))
        .filter(|(_, x)| *x)
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        *max
    } else {
        *candidates[0].0
    }
}

fn step(cups_0: &[i32]) -> Vec<i32> {
    let current = cups_0[0];
    let cups_1 = cups_0.iter().cloned().skip(1).collect::<Vec<_>>();
    let mut picked = cups_1.iter().cloned().take(3).collect::<Vec<_>>();
    let cups_2 = cups_1.iter().cloned().skip(3).collect::<Vec<_>>();
    let destination = find_destination(&current, &cups_2);
    let cups_3 = cups_2
        .iter()
        .cloned()
        .take_while(|c| c != &destination)
        .collect::<Vec<_>>();
    let mut cups_4 = cups_2
        .iter()
        .cloned()
        .skip_while(|c| c != &destination)
        .skip(1)
        .collect::<Vec<_>>();
    let mut cups_5 = cups_3;
    cups_5.push(destination);
    cups_5.append(&mut picked);
    cups_5.append(&mut cups_4);
    cups_5.push(current);
    cups_5
}

pub fn solve(cups: &[i32]) -> String {
    let i32_to_u32 = |num: &i32| u32::try_from(*num).unwrap();
    let mut cups = cups.to_owned();
    let result = iter::repeat_with(|| {
        let temp = cups.to_owned();
        cups = step(&cups);
        temp
    })
    .take(ITERATIONS + 1)
    .last()
    .unwrap();
    result
        .iter()
        .cycle()
        .skip_while(|c| c != &&1)
        .skip(1)
        .take_while(|c| c != &&1)
        .map(|n| char::from_digit(i32_to_u32(n), 10).unwrap())
        .collect::<String>()
}
