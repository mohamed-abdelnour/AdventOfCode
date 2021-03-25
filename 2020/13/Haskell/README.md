# [Day 13](https://adventofcode.com/2020/day/13)

## Table of Contents

- [Expected Results](#expected-results)
  - [Puzzle Input](#puzzle-input)
  - [Samples](#samples)

## Expected Results

### Puzzle Input

```console
$ stack exec -- Main ../input.txt
File: ../input.txt
  Part 1: 3246
  Part 2: 1010182346291467
```

### Samples

```console
$ stack exec -- Main ../sample1.txt
File: ../sample1.txt
  Part 1: 295
  Part 2: 1068781
```

```console
$ stack exec -- ghci src/Main.hs
λ> prep x = [['0'], x]
λ> solve = getRemainder . baseTimes 0 . snd . parseInput "," . prep
λ> map solve . lines <$> readFile "../samples.txt"
[3417,754018,779210,1261476,1202161486]
```
