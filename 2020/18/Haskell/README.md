# [Day 18](https://adventofcode.com/2020/day/18)

## Table of Contents

- [Credits](#credits)
- [Expected Results](#expected-results)
  - [Puzzle Input](#puzzle-input)
  - [Samples](#samples)

## Credits

- This implementation of the shunting-yard algorithm is a refactored version of
  [Haskell - infix to postfix]["haskell-infix-to-postfix"] by
  [Delerina H][delerina-h].

## Expected Results

### Puzzle Input

```console
$ stack exec -- Main ../input.txt
File: ../input.txt
  Part 1: 11297104473091
  Part 2: 185348874183674
```

### Samples

```console
$ stack ghci
λ> solve 1 . filter (not . null) . lines <$> readFile "../samples.txt"
[71,51,26,437,12240,13632]
λ> solve 2 . filter (not . null) . lines <$> readFile "../samples.txt"
[231,51,46,1445,669060,23340]
```

["haskell-infix-to-postfix"]: https://gist.github.com/Delerina/d04fc60d6ad4fd6330e3
[delerina-h]: https://gist.github.com/Delerina
