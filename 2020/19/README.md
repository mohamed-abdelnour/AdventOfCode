# [Day 19](https://adventofcode.com/2020/day/19)

## Table of Contents

- [Dependencies](#dependencies)
- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Samples](#samples)

## Dependencies

- [regex-pcre]
- [unordered-containers]
- [utf8-string]

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Part 1: 213
  Part 2: 325
```

### Samples

```console
$ ghci Main.hs
λ> part1 . lines <$> readFile "sample1.txt"
2
$ ./Main sample2.txt
File: sample2.txt
  Part 1: 3
  Part 2: 12
```

[regex-pcre]: https://hackage.haskell.org/package/regex-pcre
[unordered-containers]: https://hackage.haskell.org/package/unordered-containers
[utf8-string]: http://hackage.haskell.org/package/utf8-string-1.0.2
