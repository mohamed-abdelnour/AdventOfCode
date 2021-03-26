mod parser;

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;

pub type Ticket = Vec<u64>;

#[derive(Debug)]
pub struct Range {
    lower: u64,
    upper: u64,
}

impl Range {
    pub fn new(lower: u64, upper: u64) -> Range {
        Range { lower, upper }
    }

    fn in_range(&self, num: &u64) -> bool {
        if (num >= &self.lower) && (num <= &self.upper) {
            return true;
        }
        false
    }
}

#[derive(Debug)]
pub struct Rule<'a> {
    id: &'a str,
    ranges: Vec<Range>,
}

impl<'a> Rule<'a> {
    pub fn new(id: &str, ranges: Vec<Range>) -> Rule {
        Rule { id, ranges }
    }
}

#[derive(Debug)]
pub struct Input<'a> {
    rules: Vec<Rule<'a>>,
    ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

impl<'a> Input<'a> {
    pub fn new(
        rules: Vec<Rule>,
        ticket: Ticket,
        nearby_tickets: Vec<Ticket>,
    ) -> Input {
        Input {
            rules,
            ticket,
            nearby_tickets,
        }
    }
}

fn in_either_range(num: &u64, rule: &Rule) -> bool {
    for range in &rule.ranges {
        if range.in_range(num) {
            return true;
        }
    }
    false
}

fn in_any_range(num: &u64, rules: &[Rule]) -> bool {
    for rule in rules {
        if in_either_range(num, rule) {
            return true;
        }
    }
    false
}

fn validate_fields(ticket: &[u64], rules: &[Rule]) -> bool {
    ticket.iter().all(|field| in_any_range(field, rules))
}

fn get_columns(tickets: Vec<&Ticket>) -> Vec<Ticket> {
    let mut column = Vec::new();
    let mut columns = Vec::new();
    for i in 0..tickets[0].len() {
        for ticket in &tickets {
            column.push(ticket[i]);
        }
        columns.push(column.to_owned());
        column = Vec::new();
    }
    columns
}

fn step_fields<'a>(
    tickets: Vec<&'a Ticket>,
    rules: &'a [Rule],
) -> HashMap<&'a str, Vec<usize>> {
    let mut map: HashMap<&str, Vec<usize>> = HashMap::new();
    let columns = get_columns(tickets);
    for rule in rules {
        for (index, column) in columns.iter().enumerate() {
            let valid = column.iter().all(|x| in_either_range(x, rule));
            let field = rule.id;
            if valid {
                if map.get(field).is_some() {
                    map.get_mut(field).unwrap().push(index);
                } else {
                    map.insert(field, vec![index]);
                }
            }
        }
    }
    map
}

fn field_preprocess<'a>(
    fields: &'a HashMap<&str, Vec<usize>>,
) -> Vec<(&'a &'a str, &'a Vec<usize>)> {
    let mut fields = fields.iter().collect::<Vec<_>>();
    fields.sort_unstable_by(|a, b| a.1.len().cmp(&b.1.len()));
    fields
}

fn process_fields<'a>(
    fields: &'a [(&&str, &Vec<usize>)],
) -> HashMap<&'a &'a str, &'a usize> {
    let mut set = HashSet::new();
    let mut map = HashMap::new();

    for field in fields {
        let id = field.0;
        let candidates = field.1;
        for candidate in candidates {
            if set.get(candidate).is_none() {
                set.insert(candidate);
                map.insert(id, candidate);
            }
        }
    }
    map
}

fn departure_indices(map: &HashMap<&&str, &usize>) -> Vec<usize> {
    let mut result = Vec::new();
    for field in map.iter() {
        let candidate = field.0.split_whitespace().collect::<Vec<_>>();
        if let Some(id) = candidate.get(0) {
            if id == &"departure" {
                result.push(**field.1);
            }
        }
    }
    result
}

fn part_1(input: &Input) -> u64 {
    let rules = &input.rules;
    let nearby_tickets = &input.nearby_tickets;
    let mut result = 0;
    for ticket in nearby_tickets {
        for field in ticket {
            if !in_any_range(field, rules) {
                result += field;
            }
        }
    }
    result
}

fn part_2(input: &Input) -> u64 {
    let rules = &input.rules;
    let ticket = &input.ticket;
    let nearby_tickets = &input.nearby_tickets;
    let valid = nearby_tickets
        .iter()
        .filter(|ticket| validate_fields(ticket, rules))
        .collect::<Vec<_>>();
    let fields = step_fields(valid, rules);
    let fields = field_preprocess(&fields);
    let processed_fields = process_fields(&fields);
    let indices = departure_indices(&processed_fields);
    indices.iter().map(|x| ticket[*x]).product()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parser::parse_input(&input);

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
