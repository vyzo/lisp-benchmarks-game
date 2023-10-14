;;; -*- Gerbil -*-
;;; © vyzo
;;; fasta program from Computer Language Benchmarks Game
;;; orignated with a port of the racket version, but is now *heavily* optimized
(import :std/iter
        :std/sugar
        :std/os/fdio
        :std/text/utf8
        :gerbil/gambit)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")
(include "int.ss")
(include "flonum.ss")

(def +alu+
  (string-append
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(def +iub+
  '((#\a . 0.27) (#\c . 0.12) (#\g . 0.12) (#\t . 0.27) (#\B . 0.02)
    (#\D . 0.02) (#\H . 0.02) (#\K . 0.02) (#\M . 0.02) (#\N . 0.02)
    (#\R . 0.02) (#\S . 0.02) (#\V . 0.02) (#\W . 0.02) (#\Y . 0.02)))

(def +homosapien+
  '((#\a . 0.3029549426680) (#\c . 0.1979883004921)
    (#\g . 0.1975473066391) (#\t . 0.3015094502008)))

(def IA 3877)
(def IC 29573)
(def IM 139968)
(def SEED 42)
(def +precomputed+
  (let ((precomputed (make-vector IM))
        (seed SEED))
    (for (i (in-range IM))
      (set! seed (% (+ IC (* seed IA)) IM))
      (vector-set! precomputed i seed))
    precomputed))
(def +next+ 0)

(def (random-next!)
  (let (next (vector-ref +precomputed+ +next+))
    (set! +next+ (% (+ +next+ 1) IM))
    next))

(defregister cumulative)
(def (make-cumulative-table frequency-table)
  (fl!= cumulative 0)
  (let* ((len   (length frequency-table))
         (cdf   (make-f64vector len))
         (chars (make-u8vector len)))
    (for ((x frequency-table)
          (i (in-range len)))
      (let (xx (cdr x))
        (fl!= cumulative (+ cumulative (* xx (->fl IM))))
        (fl!= (@ cdf i) cumulative))
      (u8vector-set! chars  i (char->integer (car x))))
    (cons cdf chars)))

(defregister random)
(def (select-random cumulative-table)
  (declare (not interrupts-enabled))
  (let ((cdf (car cumulative-table))
        (chars (cdr cumulative-table))
        (rand (random-next!)))
    (fl!= random (->fl rand))
    (if (fl!? > random (@ cdf 0))
      (let loop ((i 1))
        (if (fl!? <= random (@ cdf i))
          (u8vector-ref chars i)
          (loop (+ i 1))))
      (u8vector-ref chars 0))))

(def (random-fasta head n cumulative-table)
  (write-output-line head)
  (while (> n 0)
    (for (_ (in-range (min n +line-size+)))
      (write-output-u8 (select-random cumulative-table)))
    (write-output-newline)
    (set! n (- n +line-size+))))

(def (repeat-fasta head n sequence)
  (let (seqlen (string-length sequence))
    (write-output-line head)
    (let loop-o ((n n) (k 0))
      (when (> n 0)
        (let (m (min n +line-size+))
          (let loop-i ((i 0) (k k))
            (if (< i m)
              (let (k (if (= k seqlen) 0 k))
                (write-output-char (string-ref sequence k))
                (loop-i (+ i 1) (+ k 1)))
              (begin
                (write-output-newline)
                (loop-o (- n +line-size+) k)))))))))

(def +line-size+ 60)

(def (main n)
  (let (n (string->number n))
    (repeat-fasta ">ONE Homo sapiens alu" (* n 2) +alu+)

    (random-fasta ">TWO IUB ambiguity codes" (* n 3)
                  (make-cumulative-table +iub+))

    (random-fasta ">THREE Homo sapiens frequency" (* n 5)
                  (make-cumulative-table +homosapien+))

    (flush-output)))
