use std::env;
use std::fs;

const ITERATIONS_1: usize = 2020;
const ITERATIONS_2: usize = 30_000_000;

type Game = Vec<(usize, usize)>;

fn parse_line(line: &str) -> Vec<usize> {
    line.split(',')
        .map(|x| {
            if let Some(y) = x.strip_suffix('\n') {
                y.parse().unwrap()
            } else {
                x.parse().unwrap()
            }
        })
        .collect()
}

fn parse_input(input: &str) -> Vec<Vec<usize>> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|y| parse_line(y))
        .collect()
}

fn init_game(numbers: &[usize], mut game: Game) -> Game {
    for (index, number) in numbers.iter().enumerate() {
        let index = index + 1;
        game[*number] = (index, index);
    }
    game
}

fn step_game(
    offset: usize,
    end: usize,
    mut current: usize,
    mut game: Game,
) -> usize {
    let mut before_last;
    let mut last;
    let mut delta;

    for round in offset..=end {
        before_last = game[current].0;
        last = game[current].1;

        delta = last - before_last;

        before_last = game[delta].0;
        last = game[delta].1;

        before_last = {
            if before_last == 0 {
                round
            } else {
                last
            }
        };
        last = round;

        game[delta] = (before_last, last);

        current = delta;
    }

    current
}

fn solve(numbers: &[usize], iterations: &usize) -> usize {
    let game = vec![(0, 0); *iterations];
    let offset = numbers.len() + 1;
    let first = numbers.last().unwrap();

    let game = init_game(&numbers, game);

    step_game(offset, *iterations, *first, game)
}

fn show(message: &str, iterations: usize, input: &[Vec<usize>]) {
    let solver = |iterations| {
        input
            .iter()
            .map(|x| solve(x, &iterations))
            .collect::<Vec<_>>()
    };

    let printer = |vector: Vec<usize>| {
        if vector.len() == 1 {
            println!("  Part {}: {}", message, vector[0]);
        } else {
            println!("  Part {}: {:?}", message, vector);
        }
    };

    let solution = solver(iterations);
    printer(solution);
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    show("1", ITERATIONS_1, &parsed);
    show("2", ITERATIONS_2, &parsed);
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
