# [Day 16](https://adventofcode.com/2020/day/16)

## Table of Contents

- [Dependencies](#dependencies)
- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Samples](#samples)

## Dependencies

- [text]
- [unordered-containers]

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Part 1: 23954
  Part 2: 453459307723
```

### Samples

```console
$ ghci Main.hs
λ> part1 <$> readFile "sample1.txt"
71
λ> import Data.List
λ> sample2 <- readFile "sample2.txt"
λ> (r, _, ts) = parseInput . separateInput $ sample2
λ> processedFields = assignFields r ts
λ> post = map (\(a, b) -> (b + 1, a)) . sortBy (\(_, a) (_, b) -> compare a b)
λ> post processedFields
[(1,"row"),(2,"class"),(3,"seat")]
```

[text]: https://hackage.haskell.org/package/text
[unordered-containers]: https://hackage.haskell.org/package/unordered-containers
