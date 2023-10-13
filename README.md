# LISP Benchmarks Game
This is my work in progress evaluation of Gerbil in the programming
language benchmark game.

I wanted to have a benchmark suite to analyze the progress in Gerbil
compiler development. The
[r7rs-benchmarks](https://vyzo.github.io/r7rs-benchmarks/) are not
quite satisfactory for my purposes, because there is no idiomatic
Gerbil code to optimize, just vanilla Scheme code.

I include the Racket programs to see how well Gerbil performs compared
to the most popular LISP dialect. I also include Go programs for
reference as to how well Gerbil performs compared to the industry
standard systems programming language. And I finally include C
programs, because that's the `c` of computing.

## Origin and License

The original programs for other languages originated from
https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html
and as such are licensed under the 3-clause BSD license.

This was originally based on the now defunct Alioth Programming Language Shootout,
aka The Benchmark Games.  Once I posted the site, it was brought to my attention
that the Game is _still active_.

Mea culpa, I have updated the programs to use thelatest programs from the official game site.

The Gerbil programs are coming from vyzo's keyboard and are licensed
under the MIT license.

## How to run the benchmarks
You need:
- Gerbil v0.18-rc1 or later
- Racket; I tested with v8.6
- Go; I tested with 1.21.2
- GCC; I tested with 11.4.0

To run:
```shell
$ ./prepare.sh
$ ./run-all.sh
```

## Results

See https://vyzo.github.io/lisp-benchmarks-game/
