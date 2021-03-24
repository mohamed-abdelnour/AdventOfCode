use std::convert::TryFrom;
use std::env;
use std::fs;

type Row = Vec<char>;
type Grid = Vec<Row>;
type Point = (usize, usize);

fn parse_input(input: &str) -> Grid {
    let input = input.lines().filter(|x| !x.is_empty()).collect::<Vec<_>>();
    input
        .iter()
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>()
}

fn get_seat(point: &Point, grid: &[Row]) -> char {
    let (x, y) = point;
    grid[*x][*y]
}

fn get_bounds(grid: &[Row]) -> Point {
    (grid.len() - 1, grid[0].len() - 1)
}

fn to_isize(num: &usize) -> isize {
    isize::try_from(*num).unwrap()
}

fn to_usize(num: &isize) -> usize {
    usize::try_from(*num).unwrap()
}

fn get_adjacent(grid: &[Row], point: &Point) -> Vec<Point> {
    let (x, y) = get_bounds(grid);
    let x = to_isize(&x);
    let y = to_isize(&y);
    let (px, py) = point;
    let px = to_isize(px);
    let py = to_isize(py);
    let mut adjacent = Vec::new();
    for r in (px - 1)..=(px + 1) {
        for c in (py - 1)..=(py + 1) {
            if (r >= 0)
                && (r <= x)
                && (c >= 0)
                && (c <= y)
                && ((r, c) != (px, py))
            {
                let qx = to_usize(&r);
                let qy = to_usize(&c);
                adjacent.push((qx, qy));
            }
        }
    }
    adjacent
}

fn north(point: &Point, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for r in (0..*px).rev() {
        if get_seat(&(r, *py), grid) != '.' {
            return vec![(r, *py)];
        }
    }
    vec![]
}

fn south(point: &Point, max_x: usize, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for r in (px + 1)..=max_x {
        if get_seat(&(r, *py), grid) != '.' {
            return vec![(r, *py)];
        }
    }
    vec![]
}

fn east(point: &Point, max_y: usize, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for c in (py + 1)..=max_y {
        if get_seat(&(*px, c), grid) != '.' {
            return vec![(*px, c)];
        }
    }
    vec![]
}

fn west(point: &Point, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for c in (0..*py).rev() {
        if get_seat(&(*px, c), grid) != '.' {
            return vec![(*px, c)];
        }
    }
    vec![]
}

fn north_west(point: &Point, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for r in (0..*px).rev() {
        for c in (0..*py).rev() {
            if ((px - r) == (py - c)) && (get_seat(&(r, c), grid) != '.') {
                return vec![(r, c)];
            }
        }
    }
    vec![]
}

fn south_west(point: &Point, max_x: usize, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for r in (px + 1)..=max_x {
        for c in (0..*py).rev() {
            if ((r - px) == (py - c)) && (get_seat(&(r, c), grid) != '.') {
                return vec![(r, c)];
            }
        }
    }
    vec![]
}

fn north_east(point: &Point, max_y: usize, grid: &[Row]) -> Vec<Point> {
    let (px, py) = point;
    for r in (0..*px).rev() {
        for c in (py + 1)..=max_y {
            if ((px - r) == (c - py)) && (get_seat(&(r, c), grid) != '.') {
                return vec![(r, c)];
            }
        }
    }
    vec![]
}

fn south_east(
    point: &Point,
    max_x: usize,
    max_y: usize,
    grid: &[Row],
) -> Vec<Point> {
    let (px, py) = point;
    for r in (px + 1)..=max_x {
        for c in (py + 1)..=max_y {
            if ((r - px) == (c - py)) && (get_seat(&(r, c), grid) != '.') {
                return vec![(r, c)];
            }
        }
    }
    vec![]
}

fn get_in_line_of_sight(grid: &[Row], point: &Point) -> Vec<Point> {
    let (max_x, max_y) = get_bounds(grid);
    let n = north(point, grid);
    let s = south(point, max_x, grid);
    let e = east(point, max_y, grid);
    let w = west(point, grid);
    let nw = north_west(point, grid);
    let sw = south_west(point, max_x, grid);
    let ne = north_east(point, max_y, grid);
    let se = south_east(point, max_x, max_y, grid);
    vec![n, s, e, w, nw, sw, ne, se].concat()
}

fn step_grid(mode: &usize, grid: &[Row], points: &[Point]) -> Grid {
    let mut result = grid.to_owned();
    for point in points {
        let this = get_seat(point, grid);
        let occupied = {
            if mode == &1 {
                get_adjacent(&grid, &point)
            } else {
                get_in_line_of_sight(&grid, &point)
            }
        };
        let occupied =
            occupied.iter().filter(|x| get_seat(x, grid) == '#').count();
        let (x, y) = point;
        if (this == 'L') && (occupied == 0) {
            result[*x][*y] = '#';
        } else if (this == '#') && (occupied >= mode + 3) {
            result[*x][*y] = 'L';
        }
    }
    result
}

fn cycle_grid(mode: &usize, grid: &[Row], points: &[Point]) -> Grid {
    let mut this = grid.to_owned();
    let mut next;
    loop {
        next = step_grid(mode, &this, points);
        if next == this {
            break;
        }
        this = next;
    }
    next
}

fn initial_positions(point: &Point) -> Vec<Point> {
    let mut positions = Vec::new();
    let (px, py) = *point;
    for x in 0..=px {
        for y in 0..=py {
            positions.push((x, y));
        }
    }
    positions
}

fn solve(mode: &usize, grid: &[Row]) -> usize {
    let bounds = get_bounds(grid);
    let positions = initial_positions(&bounds);
    let solved = cycle_grid(mode, grid, &positions);
    solved.concat().iter().filter(|x| *x == &'#').count()
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    println!("File: {}", path);
    println!("  Part 1: {}", solve(&1, &parsed));
    println!("  Part 2: {}", solve(&2, &parsed));
}

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|x| output(x));
}
