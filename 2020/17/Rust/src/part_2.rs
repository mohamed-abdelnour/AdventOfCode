use std::collections::{HashMap, HashSet};

type Point = (i32, i32, i32, i32);
type Grid = HashMap<Point, char>;

pub fn init_grid(input: &str) -> Grid {
    let mut x = 0;
    let mut y = 0;
    let z = 0;
    let w = 0;
    let mut grid: Grid = HashMap::new();
    for hyper_cube in input.chars() {
        if hyper_cube == '\n' {
            x += 1;
            y = 0;
        } else {
            grid.insert((x, y, z, w), hyper_cube);
            y += 1;
        }
    }
    grid
}

fn grid_lookup(point: &Point, grid: &Grid) -> char {
    if let Some(c) = grid.get(point) {
        return *c;
    }
    '.'
}

fn get_neighbours(point: &Point) -> HashSet<Point> {
    let (x, y, z, w) = point;
    let neighbouring = |t: &i32| t - 1..=t + 1;
    let mut neighbours = HashSet::new();
    for a in neighbouring(x) {
        for b in neighbouring(y) {
            for c in neighbouring(z) {
                for d in neighbouring(w) {
                    if (&a, &b, &c, &d) != (x, y, z, w) {
                        neighbours.insert((a, b, c, d));
                    }
                }
            }
        }
    }
    neighbours
}

fn count_active_neighbours(grid: &Grid, points: &HashSet<Point>) -> usize {
    points
        .iter()
        .filter(|p| grid_lookup(p, grid) == '#')
        .count()
}

fn step_grid(grid: &Grid, points: &HashSet<Point>) -> Grid {
    let mut next_grid = grid.to_owned();
    for point in points {
        let hyper_cube = grid_lookup(point, grid);
        let neighbours = get_neighbours(point);
        let active = count_active_neighbours(grid, &neighbours);
        if (hyper_cube == '#') && !(2..=3).contains(&active) {
            next_grid.insert(*point, '.');
        } else if (hyper_cube == '.') && (active == 3) {
            next_grid.insert(*point, '#');
        }
    }
    next_grid
}

pub fn cycle_grid(grid: &Grid) -> usize {
    let mut next_grid = grid.to_owned();
    for _ in 0..6 {
        let mut next_keys = next_grid.keys().cloned().collect::<HashSet<_>>();
        for key in next_grid.keys() {
            for next_key in get_neighbours(key) {
                next_keys.insert(next_key);
            }
        }
        next_grid = step_grid(&next_grid, &next_keys);
    }
    next_grid.values().filter(|c| c == &&'#').count()
}
