# [Day 09](https://adventofcode.com/2020/day/9)

## Table of Contents

- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Sample](#sample)

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Part 1: 257342611
  Part 2: 35602097
```

### Sample

```console
$ ghci Main.hs
λ> part1 5 . filter (not . null) . lines <$> readFile "sample.txt"
127
λ> part2 5 . filter (not . null) . lines <$> readFile "sample.txt"
62
```
