# [Day 05](https://adventofcode.com/2020/day/5)

## Table of Contents

- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Samples](#samples)

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Part 1:  994
  Part 2:  741
```

### Samples

```console
$ ghci Main.hs
λ> map part1 . filter (not . null) . lines <$> readFile "samples.txt"
[357,567,119,820]
```
