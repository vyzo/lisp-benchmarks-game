;;; -*- Gerbil -*-
;;; © vyzo
;;; fasta program from Computer Language Benchmarks Game
;;; orignated with a port of the racket version, but is now *heavily* optimized
(import :std/iter
        :std/sugar
        :std/os/fdio
        :std/text/utf8)
(export main)
(declare
  (not safe)
  (fixnum))

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
(def SCALE-BITS 28)
(def SCALE (expt 2 SCALE-BITS))
(def SCALE-fl (fixnum->flonum SCALE))
(def +seed+ 42)
(def (random-next!)
  (set! +seed+ (remainder (+ IC (* +seed+ IA)) IM))
  (quotient (* +seed+ SCALE) IM))

(def (make-cumulative-table frequency-table)
  (let* ((cumulative 0)
         (len (length frequency-table))
         (result (make-vector len))
         (chars  (make-string len)))
    (for ((x frequency-table)
          (i (in-range len)))
      (set! cumulative (+ cumulative (##flonum->fixnum (fl* SCALE-fl (cdr x)))))
      (vector-set! result i cumulative)
      (string-set! chars  i (car x)))
    (cons result chars)))

(def (select-random cumulative-table)
  (declare (not interrupts-enabled))
  (let ((table (car cumulative-table))
        (chars (cdr cumulative-table))
        (rand  (random-next!)))
    (let loop ((i 0))
      (if (<= rand (vector-ref table i))
        (string-ref chars i)
        (loop (+ i 1))))))

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

(def (random-fasta head n cumulative-table)
  (write-output-line head)
  (let ((n n))
    (while (> n 0)
      (for (_ (in-range (min n +line-size+)))
        (write-output-char (select-random cumulative-table)))
      (write-output-newline)
      (set! n (- n +line-size+)))))

(def +line-size+ 60)
(def +output-size+ (expt 2 15))
(def +output-buffer+
  (make-u8vector +output-size+))
(def +output-cursor+ 0)
(def +output-fd+ 1)

(defrule (write-output-line str)
  (begin
    (write-output-string str)
    (write-output-newline)))

(defrule (write-output-u8 u8)
  (if (= +output-cursor+ +output-size+)
    (begin
      (fdwrite +output-fd+ +output-buffer+ 0 +output-size+)
      (u8vector-set! +output-buffer+ 0 u8)
      (set! +output-cursor+ 1))
    (begin
      (u8vector-set! +output-buffer+ +output-cursor+ u8)
      (set! +output-cursor+ (+ +output-cursor+ 1)))))

(defrule (write-output-string str)
  (let* ((bytes (string->utf8 str))
         (len   (u8vector-length bytes))
         (output-cursor+len (+ +output-cursor+ len)))
    (if (<= output-cursor+len +output-size+)
      (begin
        (subu8vector-move! bytes 0 len +output-buffer+  +output-cursor+)
        (set! +output-cursor+ output-cursor+len))
      (begin
        (flush-output)
        (fdwrite +output-fd+ bytes)))))

(defrule (write-output-newline)
  (write-output-char #\newline))

(defrule (write-output-char char)
  (write-output-u8 (char->integer char)))

(defrule (flush-output)
  (when (> +output-cursor+ 0)
    (fdwrite +output-fd+ +output-buffer+ 0 +output-cursor+)
    (set! +output-cursor+ 0)))

(def (main n)
  (let (n (string->number n))
    (repeat-fasta ">ONE Homo sapiens alu" (* n 2) +alu+)

    (random-fasta ">TWO IUB ambiguity codes" (* n 3)
                  (make-cumulative-table +iub+))

    (random-fasta ">THREE Homo sapiens frequency" (* n 5)
                  (make-cumulative-table +homosapien+))

    (flush-output)))
