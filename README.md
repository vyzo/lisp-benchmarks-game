# LISP Benchmarks Game
This is a rehash? evolution? something something of the classic
Alioth Benchmark Game for Gerbil and other LISPs

I wanted to have a benchmark suite to analyze the progress in Gerbil
compiler development. The
[r7rs-benchmarks](https://vyzo.github.io/r7rs-benchmarks/) are not
quite satisfactory for my purposes, because there is no idiomatic
Gerbil code to optimize, just vanilla Scheme code.

So I took some programs from the (now defunct) classic benchmarks game
and set this repo up. I include the Racket programs to see how well
Gerbil performs compared to the most popular LISP dialect. I also
include Go programs for reference as to how well Gerbil performs
compared to the industry standard systems programming language. And I
finally include C programs, because that's the `c` of computing.

## Origin and License

The original programs for other languages originated from
https://github.com/Byron/benchmarksgame-cvs-mirror/tree/master/bench
and as such are licensed under the 3-clause BSD license.

The Gerbil programs are coming from vyzo's keyboard and are licensed
under the MIT license.

## Contributing
If you think your favorite language is misrepresented because the
program is old (or simply not represented), please open a pr with a
better program. I will accept prs for Racket, Clojure, SBCL, and other
LISPs, and maybe other languages like Julia and OCaml; I don't care
about B&D languages like rust, so don't bother promoting your master.

Note that it is a non-goal for me to start a new benchmarks game
competition; this is not a d*ck swinging contest, I merely want to be
able to reason about Gerbil performance concretely with some
baselines and evaluate progress as the compiler implementation advances.

## How to run the benchmarks
You need:
- Gerbil v0.18-rc1 or later
- Racket; I tested with v8.2
- Go; I tested with 1.21.2
- GCC; I tested with 11.4.0

To run:
```shell
$ ./prepare.sh
$ ./run-all.sh
```

## Results

See https://vyzo.github.io/lisp-benchmarks-game/
