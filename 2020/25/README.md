# [Day 25](https://adventofcode.com/2020/day/25)

## Table of Contents

- [Credits](#credits)
- [Dependencies](#dependencies)
- [Expected Results](#expected-results)
    + [Puzzle Input](#puzzle-input)
    + [Sample](#sample)

## Credits

- This implementation of the modular exponentiation algorithm is based on the
implementation detailed on [Rosetta Code] [(linked here)][1] and the one
provided by the [arithmoi] package [(linked here)][2].
- This implementation of the discrete logarithm algorithm is a port of the
one detailed on [CP-Algorithms] \
[(linked here)][3].

## Dependencies

- [unordered-containers]

## Expected Results

### Puzzle Input

```console
$ ./Main input.txt
File: input.txt
  Naive:   3286137
  Modular: 3286137
```

### Sample

```console
$ ./Main sample.txt
File: sample.txt
  Naive:   14897079
  Modular: 14897079
```

[Rosetta Code]: http://rosettacode.org/wiki/Rosetta_Code
[1]: https://rosettacode.org/wiki/Modular_exponentiation#Haskell
[arithmoi]: https://hackage.haskell.org/package/arithmoi
[2]: https://hackage.haskell.org/package/arithmoi-0.11.0.1/docs/src/Math.NumberTheory.Powers.Modular.html#powMod
[CP-Algorithms]: https://cp-algorithms.com/
[3]: https://cp-algorithms.com/algebra/discrete-log.html
[unordered-containers]: https://hackage.haskell.org/package/unordered-containers
