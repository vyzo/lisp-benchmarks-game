;;; -*- Gerbil -*-
;;; © vyzo
;;; fastaredux program from Computer Language Benchmarks Game
;;; orignated with a port of the racket version of fasta, now *heavily* optimized
(import :std/iter
        :std/sugar
        :std/os/fdio
        :std/text/utf8)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")

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
(def LOOKUP-BITS 20)
(def LOOKUP (expt 2 LOOKUP-BITS))
(def LOOKUP-fl (fixnum->flonum LOOKUP))
(def +seed+ 42)
(def (random-next!)
  (let (seed (remainder (+ IC (* +seed+ IA)) IM))
    (set! +seed+ seed)
    seed))

(def (make-cumulative-table frequency-table)
  (let* ((cumulative 0.0)
         (len   (length frequency-table))
         (cdf   (make-vector len))
         (chars (make-u8vector len)))
    (for ((x frequency-table)
          (i (in-range len)))
      (set! cumulative (fl+ cumulative (cdr x)))
      (vector-set! cdf i cumulative)
      (u8vector-set! chars  i (char->integer (car x))))
    (cons cdf chars)))

(def (compute-lookup-table cumulative-table)
  (let ((cdf    (car cumulative-table))
        (chars  (cdr cumulative-table))
        (result (make-u8vector LOOKUP)))
    (defrule (cdf@ i)
      (##flonum->fixnum (fl* LOOKUP-fl (vector-ref cdf i))))
    (defrule (char@ i)
      (u8vector-ref chars i))
    (let ((cdf@i (cdf@ 0)) (char@i (char@ 0)) (j-start 0))
      (let loop ((j 0) (i 0))
        (cond
         ((< j cdf@i)
          (loop (+ j 1) i))
         ((< j LOOKUP)
          (u8vector-fill! result char@i j-start j)
          (set! j-start j)
          (let (i+1 (+ i 1))
            (set! cdf@i (cdf@ i+1))
            (set! char@i (char@ i+1))
            (loop j i+1)))
         (else
          (u8vector-fill! result char@i j-start LOOKUP)))))
    result))

(def (select-random lookup-table)
  (let* ((rand  (random-next!))
         (index (quotient (fxarithmetic-shift-left rand LOOKUP-BITS) IM)))
    (u8vector-ref lookup-table index)))

(def (repeat-fasta head n sequence)
  (let (seqlen (string-length sequence))
    (write-output-line  head)
    (let loop-o ((n n) (k 0))
      (when (> n 0)
        (let (m (min n +line-size+))
          (let loop-i ((i 0) (k k))
            (if (< i m)
              (let (k (if (= k seqlen) 0 k))
                (write-output-u8 (char->integer (string-ref sequence k)))
                (loop-i (+ i 1) (+ k 1)))
              (begin
                (write-output-newline)
                (loop-o (- n +line-size+) k)))))))))

(def (random-fasta head n cumulative-table)
  (let (lookup-table (compute-lookup-table cumulative-table))
    (write-output-line head)
    (let ((n n))
      (while (> n 0)
        (for (_ (in-range (min n +line-size+)))
          (write-output-u8 (select-random lookup-table)))
        (write-output-newline)
        (set! n (- n +line-size+))))))

(def +line-size+ 60)

(def (main n)
  (let (n (string->number n))
    (repeat-fasta ">ONE Homo sapiens alu" (* n 2) +alu+)

    (random-fasta ">TWO IUB ambiguity codes" (* n 3)
                  (make-cumulative-table +iub+))

    (random-fasta ">THREE Homo sapiens frequency" (* n 5)
                  (make-cumulative-table +homosapien+))

    (flush-output)))
