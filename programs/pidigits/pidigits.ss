;;; -*- Gerbil -*-
;;; © vyzo
;;; pidigits program from Computer Language Benchmarks Game
;;; directly ported from the C version
(import :std/error
        :std/sugar
        :std/contract
        :std/iter
        :std/text/utf8
        :std/os/fdio
        :gerbil/gambit)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")
(include "int.ss")
(include "gmp.ss")

(def n1 (mpz_new))
(def n2 (mpz_new))
(def d  (mpz_new))
(def u  (mpz_new))
(def v  (mpz_new))
(def w  (mpz_new))

(def (main n)
  (let ((n (string->number n)) (k 1) (i 0))
    (mpz_init u)
    (mpz_init v)

    (mpz_init_set_si w 0)
    (mpz_init_set_si n1 4)
    (mpz_init_set_si n2 3)
    (mpz_init_set_si d 1)

    (let/cc break
      (while #t
        (mpz_tdiv_q u n1 d)
        (mpz_tdiv_q v n2 d)

        (if (= 0 (mpz_cmp u v))
          (let (digit (mpz_get_si u))
            (write-output-u8 (+ (char->integer #\0) digit))

            (set! i (+ i 1))
            (when (= (% i 10) 0)
              (write-output-char #\tab)
              (write-output-char #\:)
              (write-output-string (number->string i))
              (write-output-newline))

            (when (= i n)
              (break))

            ;; extract
            (mpz_mul_si u u -10)
            (mpz_mul u d u)
            (mpz_mul_si n1 n1 10)
            (mpz_add n1 n1 u)
            (mpz_mul_si n2 n2 10)
            (mpz_add n2 n2 u))

          (let (k2 (* k 2))
            ;; produce
            (mpz_mul_si u n1 (- k2 1))
            (mpz_add v n2 n2)
            (mpz_mul_si w n1 (- k 1))
            (mpz_add n1 u v)
            (mpz_mul_si u n2 (+ k 2))
            (mpz_add n2  w u)
            (mpz_mul_si d d (+ k2 1))
            (set! k (+ k 1))))))

    (flush-output)))
