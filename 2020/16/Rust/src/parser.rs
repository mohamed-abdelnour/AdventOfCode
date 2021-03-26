use crate::{Input, Range, Rule, Ticket};

fn parse_range(range: &str) -> Range {
    let range = range
        .split('-')
        .map(|x| {
            x.chars()
                .filter(|y| y != &' ')
                .collect::<String>()
                .parse()
                .unwrap()
        })
        .collect::<Vec<_>>();
    Range::new(range[0], range[1])
}

fn parse_ranges(ranges: &str) -> Vec<Range> {
    ranges.split("or").map(|x| parse_range(x)).collect()
}

fn parse_rule(rule: &str) -> Rule {
    let mut rule = rule.split(':').collect::<Vec<_>>();
    let ranges = rule.pop().unwrap();
    let ranges = parse_ranges(ranges);
    let rule = rule.pop().unwrap();

    Rule::new(rule, ranges)
}

fn parse_rules(rules: Vec<&str>) -> Vec<Rule> {
    rules.iter().map(|x| parse_rule(x)).collect()
}

fn parse_ticket(ticket: &str) -> Ticket {
    ticket.split(',').map(|x| x.parse().unwrap()).collect()
}

fn parse_tickets(tickets: Vec<&str>) -> Vec<Ticket> {
    tickets.iter().skip(1).map(|x| parse_ticket(x)).collect()
}

pub fn parse_input(input: &str) -> Input {
    let mut iterator =
        input.split("\n\n").map(|x| x.lines().collect::<Vec<_>>());

    let rules = parse_rules(iterator.next().unwrap());
    let ticket = parse_tickets(iterator.next().unwrap()).pop().unwrap();
    let nearby_tickets = parse_tickets(iterator.next().unwrap());

    Input::new(rules, ticket, nearby_tickets)
}
