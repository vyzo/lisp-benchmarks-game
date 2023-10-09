;;; -*- Gerbil -*-
;;; © vyzo
;;; fast program from Computer Language Benchmarks Game
;;; orignated with a port of the racket version, but is now *heavily* optimized
(import :std/iter
        :std/sugar
        :std/os/fd
        :std/os/fdio)
(export main)
(declare
  (not safe)
  (fixnum))

(define +alu+
  (string-append
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(define +iub+
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

(def (repeat-fasta id desc n sequence)
  (let (seqlen (string-length sequence))
    (display-head  id desc)
    (let loop-o ((n n) (k 0))
      (when (> n 0)
        (let (m (min n +line-size+))
          (let loop-i ((i 0) (k k))
            (if (< i m)
              (let (k (if (= k seqlen) 0 k))
                (write-output (string-ref sequence k))
                (loop-i (+ i 1) (+ k 1)))
              (begin
                (write-newline)
                (loop-o (- n +line-size+) k)))))))))

(def (random-fasta id desc n cumulative-table)
  (display-head id desc)
  (let ((n n))
    (while (> n 0)
      (for (_ (in-range (min n +line-size+)))
        (write-output (select-random cumulative-table)))
      (write-newline)
      (set! n (- n +line-size+)))))

(def +line-size+ 60)
(def +output-size+ (expt 2 15))
(def +output-buffer+
  (make-u8vector +output-size+))
(def +output-cursor+ 0)
(def +output-fd+ (fdopen 1 'out 'stdout))

(defrule (display-head id desc)
  (begin
    (flush-output)
    (let (output
          (call-with-output-u8vector []
            (lambda (port)
              (write-string id port)
              (write-char #\space port)
              (write-string desc port)
              (newline port))))
      (fdwrite +output-fd+ output))))

(defrule (write-output char)
  (if (= +output-cursor+ +output-size+)
    (begin
      (fdwrite +output-fd+ +output-buffer+ 0 +output-size+)
      (u8vector-set! +output-buffer+ 0 (char->integer char))
      (set! +output-cursor+ 1))
    (begin
      (u8vector-set! +output-buffer+ +output-cursor+ (char->integer char))
      (set! +output-cursor+ (+ +output-cursor+ 1)))))

(defrule (write-newline)
  (write-output #\newline))

(defrule (flush-output)
  (when (> +output-cursor+ 0)
    (fdwrite +output-fd+ +output-buffer+ 0 +output-cursor+)
    (set! +output-cursor+ 0)))

(def (main n)
  (let (n (string->number n))
    (repeat-fasta ">ONE" "Homo sapiens alu" (* n 2) +alu+)

    (random-fasta ">TWO" "IUB ambiguity codes" (* n 3)
                  (make-cumulative-table +iub+))

    (random-fasta ">THREE" "Homo sapiens frequency" (* n 5)
                  (make-cumulative-table +homosapien+))

    (flush-output)))
