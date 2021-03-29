use std::collections::HashMap;
use std::env;
use std::fs;

const ITERATIONS: usize = 100;

type Point = (i32, i32);
type Grid = HashMap<Point, Tile>;

#[derive(Debug)]
enum Dir {
    East,
    SouthEast,
    SouthWest,
    West,
    NorthWest,
    NorthEast,
}

impl Dir {
    fn gen_dir(first: &char, second: &char) -> Dir {
        let direction = vec![first, second];
        if direction == vec![&'s', &'e'] {
            Dir::SouthEast
        } else if direction == vec![&'s', &'w'] {
            Dir::SouthWest
        } else if direction == vec![&'n', &'w'] {
            Dir::NorthWest
        } else if direction == vec![&'n', &'e'] {
            Dir::NorthEast
        } else {
            panic!("Invalid direction");
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tile {
    Black,
    White,
}

impl Tile {
    fn flip(&self) -> Tile {
        match self {
            Tile::Black => Tile::White,
            Tile::White => Tile::Black,
        }
    }
}

fn parse_line(line: &str) -> Vec<Dir> {
    let line = line.chars().collect::<Vec<_>>();
    let mut result = Vec::new();
    let mut i = 0;
    while i < line.len() {
        let first = line[i];
        if first == 'e' {
            result.push(Dir::East);
        } else if first == 'w' {
            result.push(Dir::West);
        } else {
            i += 1;
            let second = line[i];
            let direction = Dir::gen_dir(&first, &second);
            result.push(direction);
        }
        i += 1;
    }
    result
}

fn parse_input(input: &str) -> Vec<Vec<Dir>> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parse_line(x))
        .collect()
}

fn grid_lookup(point: &Point, grid: &Grid) -> Tile {
    if let Some(tile) = grid.get(point) {
        *tile
    } else {
        Tile::White
    }
}

fn step_grid(point: &Point, direction: &Dir) -> Point {
    let (x, y) = point;
    match direction {
        Dir::East => (x + 2, *y),
        Dir::NorthEast => (x + 1, y + 1),
        Dir::SouthEast => (x + 1, y - 1),
        Dir::West => (x - 2, *y),
        Dir::NorthWest => (x - 1, y + 1),
        Dir::SouthWest => (x - 1, y - 1),
    }
}

fn find_tile(tile: &[Dir]) -> Point {
    let mut point = (0, 0);
    for entry in tile {
        point = step_grid(&point, entry);
    }
    point
}

fn step_input(tiles: &[Vec<Dir>]) -> Grid {
    let mut grid: Grid = HashMap::new();
    for tile in tiles {
        let point = find_tile(tile);
        let next = grid_lookup(&point, &grid).flip();
        grid.insert(point, next);
    }
    grid
}

fn get_adjacent(point: &Point) -> Vec<Point> {
    let x = point.0;
    let y = point.1;
    let adjacent = |t: i32| (t - 1)..=(t + 1);
    let mut result = Vec::new();
    for a in adjacent(x) {
        for b in adjacent(y) {
            if a != x && b != y {
                result.push((a, b))
            }
        }
    }
    result.push((x - 2, y));
    result.push((x + 2, y));
    result
}

fn next_tile(point: &Point, grid: &Grid) -> Tile {
    let tile = grid_lookup(point, grid);
    let adjacent = get_adjacent(point);
    let count = adjacent
        .iter()
        .filter(|x| grid_lookup(x, grid) == Tile::Black)
        .count();
    if tile == Tile::White && count == 2 {
        Tile::Black
    } else if tile == Tile::Black && (count == 0 || count > 2) {
        Tile::White
    } else {
        tile
    }
}

fn update_tiles(grid: &Grid) -> Grid {
    let mut next_grid = HashMap::new();
    let points = grid.keys();
    for point in points {
        let next = next_tile(point, grid);
        next_grid.insert(*point, next);
    }
    next_grid
}

fn cycle_tiles(grid: &Grid) -> Grid {
    let mut grid = grid.to_owned();
    for _ in 0..ITERATIONS {
        let keys = grid
            .iter()
            .filter(|(_, x)| x == &&Tile::Black)
            .map(|(k, _)| k.to_owned());
        let not_included = keys
            .flat_map(|x| get_adjacent(&x))
            .filter(|x| !grid.contains_key(x))
            .map(|x| (x, Tile::White))
            .collect::<HashMap<_, _>>();
        grid.extend(not_included);
        grid = update_tiles(&grid);
    }
    grid
}

fn solve(input: &[Vec<Dir>]) -> (usize, usize) {
    let count_black_tiles =
        |grid: &Grid| grid.values().filter(|x| x == &&Tile::Black).count();

    let stepped = step_input(&input);
    let part_1 = count_black_tiles(&stepped);

    let cycled = cycle_tiles(&stepped);
    let part_2 = count_black_tiles(&cycled);

    (part_1, part_2)
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);

    let (part_1, part_2) = solve(&parsed);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1);
    println!("  Part 2: {}", part_2);
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
