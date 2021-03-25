use std::env;
use std::fs;

#[derive(Debug)]
struct Action {
    dir: char,
    num: i32,
}

impl Action {
    fn gen(d: char, n: i32) -> Action {
        Action { dir: d, num: n }
    }
}

#[derive(Clone, Copy, Debug)]
struct Position {
    x: i32,
    y: i32,
    r: i32,
}

impl Position {
    fn gen(px: i32, py: i32, pr: i32) -> Position {
        Position {
            x: px,
            y: py,
            r: pr,
        }
    }

    fn manhattan(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

fn parse_input(input: &str) -> Vec<Action> {
    let input = input.lines().filter(|x| !x.is_empty()).collect::<Vec<_>>();
    input
        .iter()
        .map(|x| {
            let d = x.chars().take(1).collect::<Vec<_>>();
            let n = x.chars().skip(1).collect::<String>();
            Action {
                dir: d[0],
                num: n.parse::<i32>().unwrap(),
            }
        })
        .collect()
}

fn next_dir(r: i32) -> char {
    let r = r.rem_euclid(360);
    if r == 0 {
        return 'E';
    } else if r == 90 {
        return 'N';
    } else if r == 180 {
        return 'W';
    }
    'S'
}

fn move_entity(position: &Position, action: &Action) -> Position {
    let px = position.x;
    let py = position.y;
    let pr = position.r;

    let k = action.dir;
    let v = action.num;

    if k == 'N' {
        return Position::gen(px, py + v, pr);
    } else if k == 'S' {
        return Position::gen(px, py - v, pr);
    } else if k == 'E' {
        return Position::gen(px + v, py, pr);
    } else if k == 'W' {
        return Position::gen(px - v, py, pr);
    } else if k == 'L' {
        return Position::gen(px, py, pr + v);
    } else if k == 'R' {
        return Position::gen(px, py, pr - v);
    }

    let next_k = next_dir(pr);
    let next = Action::gen(next_k, v);
    move_entity(position, &next)
}

fn next_entity(v: i32, px: i32, py: i32, dir: char) -> (i32, i32) {
    let v = v.rem_euclid(360);

    let negate = |x: (i32, i32)| (-x.0, -x.1);
    let transform = |k: char, x: (i32, i32)| {
        if k == 'R' {
            x
        } else {
            negate(x)
        }
    };

    if v == 0 {
        return (px, py);
    } else if v == 180 {
        return (-px, -py);
    } else if v == 90 {
        let candidate = (py, -px);
        return transform(dir, candidate);
    }
    let candidate = (-py, px);
    transform(dir, candidate)
}

fn move_waypoint(position: &Position, action: &Action) -> Position {
    let px = position.x;
    let py = position.y;
    let pr = position.r;

    let k = action.dir;
    let v = action.num;

    let (x, y) = next_entity(v, px, py, k);

    if (k == 'R') || (k == 'L') {
        return Position::gen(x, y, pr + v);
    }

    move_entity(position, action)
}

fn step_ship(mut position: Position, actions: &[Action]) -> Position {
    for action in actions {
        position = move_entity(&position, action);
    }
    position
}

fn ship_to_waypoint(positions: &[Position], n: i32) -> Vec<Position> {
    let ship = &positions[0];
    let xs = ship.x;
    let ys = ship.y;
    let rs = ship.r;

    let waypoint = &positions[1];
    let xw = waypoint.x;
    let yw = waypoint.y;
    let rw = waypoint.r;

    let next = Position::gen(xs + n * xw, ys + n * yw, rs + n * rw);

    vec![next, *waypoint]
}

fn step_waypoint(mut positions: Vec<Position>, actions: &[Action]) -> Position {
    for action in actions {
        let ship = positions[0];
        let waypoint = positions[1];
        let k = action.dir;
        let v = action.num;
        if k != 'F' {
            positions = vec![ship, move_waypoint(&waypoint, action)];
        } else {
            positions = ship_to_waypoint(&positions, v);
        }
    }
    positions[0]
}

fn part_1(input: &[Action]) -> i32 {
    let initial = Position::gen(0, 0, 0);
    step_ship(initial, input).manhattan()
}

fn part_2(input: &[Action]) -> i32 {
    let initial_ship = Position::gen(0, 0, 0);
    let initial_waypoint = Position::gen(10, 1, 0);
    let initial = vec![initial_ship, initial_waypoint];
    step_waypoint(initial, input).manhattan()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

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
