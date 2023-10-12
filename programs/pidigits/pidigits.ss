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
(include "gmp.ss")

(def tmp1 (mpz_new))
(def tmp2 (mpz_new))
(def acc  (mpz_new))
(def den  (mpz_new))
(def num  (mpz_new))

(def (extract-digit n)
  (mpz_mul_ui tmp1 num n)
  (mpz_add tmp2 tmp1 acc)
  (mpz_tdiv_q tmp1 tmp2 den)

  (mpz_get_ui tmp1))

(def (eliminate-digit d)
  (mpz_submul_ui acc den d)
  (mpz_mul_ui acc acc 10)
  (mpz_mul_ui num num 10))

(def (next-term! k)
  (let (k2 (+ (* k 2) 1))
    (mpz_addmul_ui acc num 2)
    (mpz_mul_ui acc acc k2)
    (mpz_mul_ui den den k2)
    (mpz_mul_ui num num k)))

(def (main n)
  (let ((n (string->number n))
        (k 0)
        (i 0))
    (mpz_init tmp1)
    (mpz_init tmp2)

    (mpz_init_set_ui acc 0)
    (mpz_init_set_ui den 1)
    (mpz_init_set_ui num 1)

    (while (< i n)
      (set! k (+ k 1))
      (next-term! k)

      (unless (> (mpz_cmp num acc) 0)
        (let (d (extract-digit 3))
          (when (= d (extract-digit 4))
            (write-output-u8 (+ (char->integer #\0) d))
            (set! i (+ i 1))
            (when (= (remainder i 10) 0)
              (write-output-char #\tab)
              (write-output-char #\:)
              (write-output-string (number->string i))
              (write-output-newline))
            (eliminate-digit d)))))
    (flush-output)))
