use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;

type FoodMap = HashMap<usize, Vec<String>>;
type AllergenMap = HashMap<String, Vec<usize>>;

fn parse_line(line: &str) -> (Vec<String>, Vec<&str>) {
    let line = line.split_whitespace().collect::<Vec<_>>();
    let line = line.split(|x| x == &"(contains").collect::<Vec<_>>();
    let mut foods = vec![];
    let mut allergens = vec![];
    if let Some(f) = line.get(0) {
        foods = f.iter().map(|x| x.to_string()).collect();
    }
    if let Some(a) = line.get(1) {
        allergens = a
            .iter()
            .map(|t| t.trim_end_matches(|c| c == ')' || c == ','))
            .collect();
    }
    (foods, allergens)
}

fn parse_input(input: &str) -> (FoodMap, AllergenMap) {
    let mut food_map = HashMap::new();
    let mut allergen_map: AllergenMap = HashMap::new();
    for (index, line) in input.lines().enumerate() {
        let (foods, allergens) = parse_line(line);
        food_map.insert(index, foods);
        for allergen in allergens {
            if let Some(indices) = allergen_map.get_mut(allergen) {
                indices.push(index);
            } else {
                allergen_map.insert(allergen.to_string(), vec![index]);
            }
        }
    }
    (food_map, allergen_map)
}

fn to_remove(
    food_map: &FoodMap,
    allergen_map: &AllergenMap,
) -> (HashMap<String, Vec<String>>, HashSet<String>) {
    let mut allergen_set = HashMap::new();
    let mut set_to_remove = HashSet::new();
    let mut foods_to_remove = Vec::new();
    for (allergen, index) in allergen_map.iter() {
        let mut food_tracker = HashMap::new();
        for occurrence in index {
            let foods = food_map.get(occurrence).unwrap();
            for food in foods {
                if let Some(count) = food_tracker.get_mut(food) {
                    *count += 1;
                } else {
                    food_tracker.insert(food.to_string(), 1);
                }
            }
        }
        food_tracker
            .iter()
            .filter(|(_, v)| v == &&index.len())
            .for_each(|(k, _)| {
                foods_to_remove.push(k.to_owned());
                set_to_remove.insert(k.to_owned());
            });
        allergen_set.insert(allergen.to_string(), foods_to_remove);
        foods_to_remove = Vec::new();
    }
    (allergen_set, set_to_remove)
}

fn match_allergens(
    set_to_match: &[(String, Vec<String>)],
) -> Vec<(String, String)> {
    let mut set_to_match = set_to_match.to_owned();
    let mut matches = HashSet::new();
    let mut result = Vec::new();
    while !set_to_match.is_empty() {
        let (allergen, candidates) = set_to_match.remove(0);
        if candidates.len() == 1 {
            let candidate = &candidates[0];
            result.push((allergen.to_string(), candidate.to_owned()));
            matches.insert(candidate.to_owned());
        } else {
            let next = candidates
                .iter()
                .cloned()
                .filter(|c| !matches.contains(&c.to_string()))
                .collect::<Vec<_>>();
            set_to_match.push((allergen, next));
        }
    }
    result
}

fn solve(input: &str) -> (usize, String) {
    let (food_map, allergen_map) = parse_input(input);
    let (allergen_set, set_to_remove) = to_remove(&food_map, &allergen_map);
    let part_1 = food_map
        .values()
        .flatten()
        .filter(|x| !set_to_remove.contains(&x.to_string()))
        .count();
    let mut allergen_set = allergen_set
        .iter()
        .map(|(a, b)| (a.to_owned(), b.to_owned()))
        .collect::<Vec<_>>();
    allergen_set.sort_unstable_by(|(_, a), (_, b)| a.len().cmp(&b.len()));
    let mut matched = match_allergens(&allergen_set);
    matched.sort_unstable_by(|(a, _), (b, _)| a.cmp(&b));
    let first = matched[0].1.to_string();
    let part_2 = matched.iter().skip(1).fold(first, |mut acc, (_, x)| {
        acc.push(',');
        acc.push_str(x);
        acc
    });
    (part_1, part_2)
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();

    let (part_1, part_2) = solve(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1);
    println!("  Part 2: {}", part_2);
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
