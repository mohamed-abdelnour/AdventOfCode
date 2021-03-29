use std::cmp::Ordering;
use std::collections::HashSet;
use std::env;
use std::fs;

type Deck = Vec<usize>;
type GameStates = HashSet<(Deck, Deck)>;

fn init_player(player: &str) -> Deck {
    player.lines().skip(1).map(|x| x.parse().unwrap()).collect()
}

fn parse_input(input: &str) -> (Deck, Deck) {
    let input = input
        .split("\n\n")
        .filter(|x| !x.is_empty())
        .map(|x| init_player(x))
        .collect::<Vec<_>>();
    let player_1 = input[0].to_owned();
    let player_2 = input[1].to_owned();
    (player_1, player_2)
}

fn combat(players: &(Deck, Deck)) -> Deck {
    let mut player_1 = players.0.to_owned();
    let mut player_2 = players.1.to_owned();
    loop {
        if player_2.is_empty() {
            return player_1;
        } else if player_1.is_empty() {
            return player_2;
        } else {
            let hand_1 = player_1.remove(0);
            let hand_2 = player_2.remove(0);
            match hand_1.cmp(&hand_2) {
                Ordering::Greater => {
                    player_1.push(hand_1);
                    player_1.push(hand_2);
                }
                Ordering::Less => {
                    player_2.push(hand_2);
                    player_2.push(hand_1);
                }
                Ordering::Equal => panic!("Equal cards"),
            }
        }
    }
}

fn recursive_combat(
    mut game_states: GameStates,
    players: &(Deck, Deck),
) -> (usize, Deck) {
    let mut player_1 = players.0.to_owned();
    let mut player_2 = players.1.to_owned();
    if player_2.is_empty() {
        (1, player_1)
    } else if player_1.is_empty() {
        (2, player_2)
    } else {
        let game_state = (player_1.to_owned(), player_2.to_owned());
        if game_states.contains(&game_state) {
            (1, player_1)
        } else {
            game_states.insert(game_state);
            let hand_1 = player_1.remove(0);
            let hand_2 = player_2.remove(0);
            let winner = match hand_1.cmp(&hand_2) {
                Ordering::Greater => 1,
                Ordering::Less => 2,
                Ordering::Equal => panic!("Equal cards"),
            };
            let winner = {
                if (player_1.len() >= hand_1) && (player_2.len() >= hand_2) {
                    let player_1 =
                        player_1.iter().take(hand_1).cloned().collect::<Deck>();
                    let player_2 =
                        player_2.iter().take(hand_2).cloned().collect::<Deck>();
                    recursive_combat(HashSet::new(), &(player_1, player_2)).0
                } else {
                    winner
                }
            };
            if winner == 1 {
                player_1.push(hand_1);
                player_1.push(hand_2);
            } else {
                player_2.push(hand_2);
                player_2.push(hand_1);
            }
            recursive_combat(game_states, &(player_1, player_2))
        }
    }
}

fn solve_by<F>(predicate: F, input: &str) -> usize
where
    F: Fn(&(Deck, Deck)) -> Deck,
{
    let players = parse_input(&input);
    let game = predicate(&players);
    (1..)
        .zip(game.iter().rev())
        .fold(0, |acc, (a, b)| acc + a * b)
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();

    println!("File: {}", path);
    println!("  Part 1: {}", solve_by(combat, &input));
    println!(
        "  Part 2: {}",
        solve_by(|x| recursive_combat(HashSet::new(), x).1, &input)
    );
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
