# [Day 14](https://adventofcode.com/2020/day/14)

## Table of Contents

- [Dependencies](#dependencies)
- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Samples](#samples)

## Dependencies

- [unordered-containers]

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Part 1: 14862056079561
  Part 2: 3296185383161
```

### Samples

```console
$ ghci Main.hs
λ> P1.part1 . lines <$> readFile "sample1.txt"
165
λ> P2.part2 . lines <$> readFile "sample2.txt"
208
```

[unordered-containers]: https://hackage.haskell.org/package/unordered-containers
