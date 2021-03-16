## Solutions to the [Advent of Code 2020][AoC2020] in [Haskell]

## Table of Contents

- [Prerequisites](#prerequisites)
- [Miscellaneous Tools](#miscellaneous-tools)
- [Instructions](#instructions)
- [Demonstration](#demonstration)

## Prerequisites

### [GHC]

```console
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.4
```

### Haskell Dependencies

Throughout the 25 days the dependencies used are:
- [base]
- [regex-pcre]
- [text]
- [unordered-containers]
- [utf8-string]
- [vector]

For a given day, if the solution to that day's puzzle has dependencies other
than base, they will be specified in that day's README.

Solving these problems was how I started learning Haskell, so for the sake of 
simplicity I chose not to use [cabal-install] or [stack].

It is likely for these dependencies to be available through your package
manager. There are all available in the standard [Arch] repositories and the
[Debian] repositories, respectively.

I currently have GHC and all these dependencies installed through [Nix].
The following overlay adds these dependencies to GHC's package list.

```nix
self: super: {
  ghc = super.haskellPackages.ghcWithPackages (
    haskellPackages: with haskellPackages; [
      regex-pcre
      unordered-containers
      utf8-string
      vector
    ]
  );
}
```
#### A note about the text package

The text package is commonly included with the base install of GHC; that
is the case with Arch, Debian and Nix. Thus, in order to avoid conflicts,
check if you already have it installed before trying to install it. Run the
following command to check. Getting no output indicates that the package is
not installed.

```console
$ ghc-pkg list --simple-output "text"
text-1.2.4.1
```

## Miscellaneous Tools

This is a quick overview of my set-up for solving these puzzles.
+ Editor
    - [Neovim]
+ Formatter
    - [brittany]
+ LSP
    - [Haskell Language Server][HLS]
    - [coc.nvim]
+ Linter
    - [HLint]

## Instructions

The following instructions have been tested on [Linux].

```console
$ cd [DAY]
$ ghc -O2 -Wall Main.hs
$ ./Main [PATH...]
```

If your install of GHC and Haskell packages uses dynamically linked libraries
(as with Arch), you must pass the `-dynamic` flag to GHC. For more
information, [here][HaskellArch] is the relevant page on the [ArchWiki].

## Demonstration

The following applies to all days. Day 01 is used here as a demonstration.

```console
$ cd 01
$ ghc -O2 -Wall Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
$ ./Main input.txt sample.txt
File: input.txt
  Part 1: 444019
  Part 2: 29212176
File: sample.txt
  Part 1: 514579
  Part 2: 241861950
```

The executable can be provided with multiple files (as shown above) or one
file at a time.

```console
$ ./Main input.txt
File: input.txt
  Part 1: 444019
  Part 2: 29212176
$ ./Main sample.txt
File: sample.txt
  Part 1: 514579
  Part 2: 241861950
```

[AoC2020]: https://adventofcode.com/2020/
[Haskell]: https://www.haskell.org/
[GHC]: https://www.haskell.org/ghc/
[Linux]: https://www.linux.org/

[base]: https://hackage.haskell.org/package/base
[regex-pcre]: https://hackage.haskell.org/package/regex-pcre
[text]: https://hackage.haskell.org/package/text
[unordered-containers]: https://hackage.haskell.org/package/unordered-containers
[utf8-string]: http://hackage.haskell.org/package/utf8-string-1.0.2
[vector]: https://hackage.haskell.org/package/vector

[cabal-install]: https://hackage.haskell.org/package/cabal-install
[stack]: https://hackage.haskell.org/package/stack

[Arch]: https://archlinux.org/
[Debian]: https://www.debian.org/
[Nix]: https://github.com/NixOS/nix

[Neovim]: https://github.com/neovim/neovim
[coc.nvim]: https://github.com/neoclide/coc.nvim
[HLS]: https://github.com/haskell/haskell-language-server
[HLint]: https://github.com/ndmitchell/hlint
[brittany]: https://github.com/lspitzner/brittany

[HaskellArch]: https://wiki.archlinux.org/index.php/haskell#Invoking_GHC_directly
[ArchWiki]: https://wiki.archlinux.org/
