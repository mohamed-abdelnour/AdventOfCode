use std::collections::HashMap;
use std::env;
use std::fs;

type TileId = u64;
type TileRow = Vec<char>;
type Tile = Vec<TileRow>;
type TileSet = HashMap<TileId, Tile>;
type AltTileSet = HashMap<TileId, Vec<Tile>>;

enum Dir {
    T,
    B,
    R,
    L,
    A,
}

fn parse_input(input: &str) -> Vec<Vec<&str>> {
    input
        .split("\n\n")
        .map(|x| x.lines().collect())
        .filter(|x| x != &vec![""])
        .collect()
}

fn init_tiles(tiles: &[Vec<&str>]) -> TileSet {
    let tiles = tiles.to_owned();
    let mut tile_set = HashMap::new();
    for mut tile in tiles {
        let id = tile.remove(0);
        let id = id.strip_prefix("Tile ").unwrap();
        let id = id.strip_suffix(':').unwrap();
        let id = id.parse::<u64>().unwrap();
        let tile = tile.iter().map(|t| t.chars().collect()).collect();
        tile_set.insert(id, tile);
    }
    tile_set
}

fn transpose<T: Clone>(matrix: &[Vec<T>]) -> Vec<Vec<T>> {
    (0..matrix[0].len())
        .map(|i| matrix.iter().map(|vector| vector[i].to_owned()).collect())
        .collect()
}

fn rotate<T: Clone>(matrix: &[Vec<T>]) -> Vec<Vec<T>> {
    transpose(matrix).iter().cloned().rev().collect()
}

fn apply_transforms(tile: &[TileRow]) -> Vec<Tile> {
    let mut transformed = Vec::new();
    let mut next = tile.to_owned();
    for _ in 0..4 {
        let rotated = rotate(&next);
        let flipped = rotated.iter().cloned().rev().collect();
        next = rotated.to_owned();
        transformed.push(rotated);
        transformed.push(flipped);
    }
    transformed
}

fn init_alt_tiles(tile_set: &TileSet) -> AltTileSet {
    let mut alt_tile_set = HashMap::new();
    for (key, tile) in tile_set.iter() {
        let alt_tiles = apply_transforms(tile);
        alt_tile_set.insert(*key, alt_tiles);
    }
    alt_tile_set
}

fn match_right<T: PartialEq>(input_1: &[Vec<T>], input_2: &[Vec<T>]) -> bool {
    for i in 0..input_1.len() {
        if input_1[i].last().unwrap() != input_2[i].first().unwrap() {
            return false;
        }
    }
    true
}

fn dir_match(key: &Dir, tile_1: &[TileRow], tile_2: &[TileRow]) -> bool {
    match key {
        Dir::T => tile_1.last().unwrap() == tile_2.first().unwrap(),
        Dir::B => dir_match(&Dir::T, tile_2, tile_1),
        Dir::R => match_right(tile_1, tile_2),
        Dir::L => dir_match(&Dir::R, tile_2, tile_1),
        Dir::A => {
            dir_match(&Dir::T, tile_1, tile_2)
                || dir_match(&Dir::B, tile_1, tile_2)
                || dir_match(&Dir::R, tile_1, tile_2)
                || dir_match(&Dir::L, tile_1, tile_2)
        }
    }
}

fn check_match(key: &Dir, tile: &[TileRow], tiles: &[Tile]) -> Vec<Tile> {
    tiles
        .iter()
        .cloned()
        .filter(|t| dir_match(key, tile, t))
        .collect()
}

fn match_count(key: &Dir, tile: &[TileRow], tiles: &[Tile]) -> usize {
    check_match(key, tile, tiles).len()
}

fn check_alt_matches(
    tile_set: &TileSet,
    alt_tile_set: &AltTileSet,
) -> Vec<(u64, Vec<usize>)> {
    let mut alt_matches = Vec::new();
    for key in alt_tile_set.keys() {
        let mut copied_set = tile_set.to_owned();
        let alt_tiles = alt_tile_set.get(key).unwrap();
        copied_set.remove(key);
        let values = copied_set.values().cloned().collect::<Vec<_>>();
        let matches = alt_tiles
            .iter()
            .map(|t| match_count(&Dir::A, t, &values))
            .collect::<Vec<_>>();
        alt_matches.push((*key, matches));
    }
    alt_matches
}

fn get_corners(tile_set: &TileSet, alt_tile_set: &AltTileSet) -> Vec<u64> {
    let alt_matches = check_alt_matches(tile_set, alt_tile_set);
    alt_matches.iter().fold(vec![], |mut acc, (id, matches)| {
        if matches.iter().sum::<usize>() == 2 {
            acc.push(*id);
            acc
        } else {
            acc
        }
    })
}

fn init_top_left(tile_id: &TileId, alt_tile_set: &AltTileSet) -> Tile {
    let top_left = alt_tile_set.get(tile_id).unwrap();
    let mut alt_tile_set = alt_tile_set.to_owned();
    alt_tile_set.remove(tile_id);
    let tiles = alt_tile_set.values().cloned().collect::<Vec<_>>().concat();
    for candidate in top_left {
        let matcher = |x| match_count(&x, candidate, &tiles);
        let top = matcher(Dir::T);
        let bottom = matcher(Dir::B);
        let right = matcher(Dir::R);
        let left = matcher(Dir::L);
        if (top, left, bottom, right) == (0, 0, 1, 1) {
            return candidate.to_vec();
        }
    }
    vec![]
}

fn remove_borders<T: Clone>(input: &[T]) -> Vec<T> {
    let length = input.len();
    if length > 1 {
        (1..input.len() - 1).map(|i| input[i].to_owned()).collect()
    } else {
        vec![]
    }
}

fn attach_right<T: Clone>(
    input_1: &[Vec<T>],
    input_2: &[Vec<T>],
) -> Vec<Vec<T>> {
    let mut input_1 = input_1.to_owned();
    let input_2 = input_2.to_owned();
    let mut result = Vec::new();
    for i in 0..input_2.len() {
        input_1[i].append(&mut remove_borders(&input_2[i]));
        result.push(input_1[i].to_owned());
    }
    result
}

fn attach_bottom<T: Clone>(
    input_1: &[Vec<T>],
    input_2: &[Vec<T>],
) -> Vec<Vec<T>> {
    let mut input_1 = input_1.to_owned();
    let mut input_2 = input_2.to_owned();
    input_2.append(&mut input_1);
    input_2
}

fn find_match(
    dir: &Dir,
    tile: &[TileRow],
    alt_tile_set: &AltTileSet,
) -> Option<(TileId, Tile)> {
    for (key, value) in alt_tile_set.iter() {
        for candidate in value {
            if dir_match(dir, tile, candidate) {
                return Some((*key, candidate.to_vec()));
            }
        }
    }
    None
}

fn generate_row(
    tile: &[TileRow],
    alt_tile_set: &AltTileSet,
) -> (Vec<Tile>, AltTileSet) {
    let mut alt_tile_set = alt_tile_set.to_owned();
    let mut tile = tile.to_owned();
    let mut result = Vec::new();
    loop {
        if let Some(matches) = find_match(&Dir::R, &tile, &alt_tile_set) {
            let (key, value) = matches;
            tile = value.to_owned();
            result.push(value);
            alt_tile_set.remove(&key);
        } else {
            return (result, alt_tile_set);
        }
    }
}

fn fix_row(tile: &[TileRow], row: &[Tile]) -> Tile {
    let tile = tile.to_owned();
    let tile = tile.iter().map(|t| remove_borders(t)).collect::<Vec<_>>();
    let row = row.to_owned();
    let row = row.iter().fold(tile, |acc, t| attach_right(&acc, t));
    row
}

fn generate_rows(tile: &[TileRow], alt_tile_set: &AltTileSet) -> Vec<Tile> {
    let mut alt_tile_set = alt_tile_set.to_owned();
    let mut tile = tile.to_owned();
    let mut result = Vec::new();
    let mut row;
    loop {
        let generated = generate_row(&tile, &alt_tile_set);
        let r = generated.0;
        alt_tile_set = generated.1;
        row = fix_row(&tile, &r);
        if let Some(matches) = find_match(&Dir::B, &tile, &alt_tile_set) {
            let (key, value) = matches;
            tile = value.to_owned();
            result.push(remove_borders(&row));
            alt_tile_set.remove(&key);
        } else {
            result.push(remove_borders(&row));
            return result;
        }
    }
}

const MONSTER: [&str; 3] = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
];

fn monster_head() -> usize {
    MONSTER[0].len()
}

fn monster() -> Vec<Vec<char>> {
    MONSTER.iter().map(|m| m.chars().collect()).collect()
}

fn find_monster_helper(rows: &[TileRow]) -> usize {
    let mut rows = rows.to_owned();
    let mut count = 0;
    let monster = monster();
    while rows[0].len() >= monster_head() {
        let mut found = true;
        for i in 0..3 {
            let current = rows[i]
                .iter()
                .zip(monster[i].iter())
                .filter(|(_, m)| m == &&'#')
                .all(|(r, m)| r == m);
            rows[i].remove(0);
            found = found && current;
        }
        if found {
            count += 1;
        }
    }
    count
}

fn find_monsters(tile: &[TileRow]) -> usize {
    let mut count = 0;
    for i in 0..(tile.len() - 2) {
        let current =
            (i..=i + 2).map(|n| tile[n].to_owned()).collect::<Vec<_>>();
        count += find_monster_helper(&current);
    }
    count
}

fn part_1(corners: &[u64]) -> u64 {
    corners.iter().product()
}

fn part_2(alt_tile_set: &AltTileSet, corners: &[u64]) -> usize {
    let top_left_id = corners[0];
    let top_left = init_top_left(&top_left_id, alt_tile_set);

    let mut alt_tile_set = alt_tile_set.to_owned();
    alt_tile_set.remove(&top_left_id);

    let grid = generate_rows(&top_left, &alt_tile_set);
    let grid = grid.iter().fold(vec![], |acc, t| attach_bottom(&acc, t));

    let transforms = apply_transforms(&grid);
    let monsters = transforms.iter().map(|t| find_monsters(t)).max().unwrap();

    let count =
        |x: &[Vec<char>]| x.concat().iter().filter(|t| t == &&'#').count();

    let grid_count = count(&grid);
    let monster_count = count(&monster());

    grid_count - monsters * monster_count
}

#[allow(unused)]
fn show_tile(input: &[TileRow]) {
    input.iter().for_each(|t| {
        let row = t.iter().collect::<String>();
        println!("{}", row);
    });
    println!();
}

fn output(path: &str) {
    let input = fs::read_to_string(&path).unwrap();
    let parsed = parse_input(&input);
    let tile_set = init_tiles(&parsed);
    let alt_tile_set = init_alt_tiles(&tile_set);
    let corners = get_corners(&tile_set, &alt_tile_set);

    println!("File: {}", path);
    println!("  Part 1: {}", part_1(&corners));
    println!("  Part 2: {}", part_2(&alt_tile_set, &corners));
}

fn main() {
    env::args().skip(1).for_each(|x| output(&x));
}
