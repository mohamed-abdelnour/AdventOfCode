# [Day 04](https://adventofcode.com/2020/day/4)

## Table of Contents

- [Expected Results](#expected-results)
  - [Puzzle Input](#puzzle-input)
  - [Samples](#samples)

## Expected Results

### Puzzle Input

```console
$ stack exec -- Main ../input.txt
File: ../input.txt
  Part 1:  204
  Part 2:  179
```

### Samples

```console
$ stack ghci
λ> part1 . passports . lines <$> readFile "../sample.txt"
2
λ> part2 . passports . lines <$> readFile "../invalid.txt"
0
λ> part2 . passports . lines <$> readFile "../valid.txt"
4
```
