use std::convert::TryFrom;

const MAX_CUPS: usize = 1_000_000;
const ITERATIONS: usize = 10_000_000;

fn i32_to_usize(x: i32) -> usize {
    usize::try_from(x).unwrap()
}

fn init_input(cups: &[i32]) -> Vec<(i32, i32)> {
    (0..cups.len() - 1)
        .map(|i| (cups[i], cups[i + 1]))
        .collect()
}

fn init_cups(init: &[(i32, i32)], last: &i32, next: &i32) -> Vec<i32> {
    let mut init = init.to_owned();
    init.push((*last, *next));
    init.sort_unstable_by(|(x, _), (y, _)| x.cmp(y));
    init.iter().map(|(_, x)| x.to_owned()).collect()
}

fn init_game(init: &[i32], mut game: Vec<usize>) -> Vec<usize> {
    let mut offset = 0;
    for cup in init {
        game.push(i32_to_usize(*cup));
        offset += 1;
    }
    for i in (offset + 2)..=MAX_CUPS {
        game.push(i);
    }
    game
}

fn step_game(mut n_0: usize, mut game: Vec<usize>) -> Vec<usize> {
    for _ in 0..ITERATIONS {
        let p_1 = game[n_0 - 1];
        let p_2 = game[p_1 - 1];
        let p_3 = game[p_2 - 1];
        let ps = vec![p_1, p_2, p_3];
        let n_1 = game[p_3 - 1];
        game[n_0 - 1] = n_1;
        let c = {
            if !ps.contains(&(n_0 - 1)) {
                n_0 - 1
            } else if !ps.contains(&(n_0 - 2)) {
                n_0 - 2
            } else if !ps.contains(&(n_0 - 3)) {
                n_0 - 3
            } else {
                n_0 - 4
            }
        };
        let d_0 = {
            if c < 1 {
                MAX_CUPS
            } else {
                c
            }
        };
        let d_1 = game[d_0 - 1];
        game[d_0 - 1] = p_1;
        game[p_3 - 1] = d_1;
        n_0 = n_1;
    }
    game
}

pub fn solve(cups: &[i32]) -> (usize, usize, u64) {
    let init = init_input(cups);
    let head = cups.first().unwrap();
    let last = cups.last().unwrap();
    let next = cups.iter().max().unwrap() + 1;
    let init = init_cups(&init, last, &next);
    let mut game = Vec::with_capacity(MAX_CUPS);
    game = init_game(&init, game);
    game.push(i32_to_usize(*head));
    game = step_game(i32_to_usize(*head), game);
    let r_1 = game[0];
    let r_2 = game[r_1 - 1];
    (r_1, r_2, u64::try_from(r_1 * r_2).unwrap())
}
