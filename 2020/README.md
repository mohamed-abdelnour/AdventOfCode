# Solutions to the [Advent of Code 2020][aoc2020] in [Haskell][haskell] and [Rust][rust]

## Table of Contents

- [Prerequisites](#prerequisites)
- [Instructions](#instructions)
- [Demonstration](#demonstration)

## Prerequisites

### Haskell

- [Stack][stack]

### Rust

- [Cargo][cargo]

## Instructions

The following instructions have been tested on GNU/Linux.

### Haskell

```console
$ cd [DAY]/Haskell
$ stack build
$ stack exec -- Main [PATH...]
```

### Rust

```console
$ cd [DAY]/Rust
$ cargo build --release
$ cargo run -q --release -- [PATH...]
```

## Demonstration

The following applies to all days. Day 01 is used here as a demonstration.

### Haskell

```console
$ cd 01/Haskell
$ stack build
$ stack exec -- Main ../input.txt ../sample.txt
File: ../input.txt
  Part 1: 444019
  Part 2: 29212176
File: ../sample.txt
  Part 1: 514579
  Part 2: 241861950
```

### ٌRust

```console
$ cd 01/Rust
$ cargo build --release
$ cargo run -q --release -- ../input.txt ../sample.txt
File: ../input.txt
  Part 1: 444019
  Part 2: 29212176
File: ../sample.txt
  Part 1: 514579
  Part 2: 241861950
```

The executables can be provided with multiple files (as shown above) or one
file at a time.

```console
$ stack exec -- Main ../input.txt
File: ../input.txt
  Part 1: 444019
  Part 2: 29212176
```

```console
$ cargo run -q --release -- ../input.txt
File: ../input.txt
  Part 1: 444019
  Part 2: 29212176
```

[aoc2020]: https://adventofcode.com/2020/
[haskell]: https://www.haskell.org/
[rust]: https://www.rust-lang.org/
[stack]: https://docs.haskellstack.org/
[cargo]: https://doc.rust-lang.org/stable/cargo/
