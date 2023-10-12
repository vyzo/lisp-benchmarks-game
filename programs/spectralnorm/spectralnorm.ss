;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from the go version and optimized
(import :std/error
        :std/sugar
        :std/contract
        :std/iter
        :std/format
        :std/text/utf8
        :std/os/fdio
        :gerbil/gambit)
(export main)
(declare
  (not safe)
  (fixnum))
(include "io.ss")
(include "flonum.ss")

(def (eval-A i j)
  (+ (quotient (* (+ i j) (+ i j 1)) 2) i 1))

(defregister vi)
(def (times v u)
  (for (i (in-range (f64vector-length v)))
    (fl!= vi 0)
    (for (j (in-range (f64vector-length u)))
      (let (a (eval-A i j))
        (fl!= vi (+ vi (/ (@ u j) (->fl a))))))
    (fl!= (@ v i) vi)))

(def (times-transp v u)
  (for (i (in-range (f64vector-length v)))
    (fl!= vi 0)
    (for (j (in-range (f64vector-length u)))
      (let (a (eval-A j i))
        (fl!= vi (+ vi (/ (@ u j) (->fl a))))))
    (fl!= (@ v i) vi)))

(def (at-times-transp v u)
  (let (x (make-f64vector (f64vector-length u) 0.0))
    (times x u)
    (times-transp v x)))

(defregister vBv)
(defregister vv)
(defregister result)
(def (main n)
  (let* ((n (string->number n))
         (u (make-f64vector n 1.0))
         (v (make-f64vector n 0.0)))
    (for (_ (in-range 10))
      (at-times-transp v u)
      (at-times-transp u v))
    (for (i (in-range n))
      (fl!= vi  (@ v i))
      (fl!= vBv (+ vBv (* (@ u i) vi)))
      (fl!= vv  (+ vv (* (^2 vi)))))
    (fl!= result (sqrt (/ vBv vv)))
    (write-output-string (format "~0,9f~n" (register-ref result)))
    (flush-output)))
