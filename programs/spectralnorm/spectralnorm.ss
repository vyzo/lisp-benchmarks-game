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
(include "int.ss")

(def +A+ #f)
(def (precompute-A! n)
  (let (A (make-vector (* 2 n)))
    (for (i (in-range n))
      (for (j (in-range n))
        (let (i+j (+ i j))
          (let (val (+ (// (* i+j (+ i+j 1)) 2) 1))
            (vector-set! A i+j val)
            val))))
    (set! +A+ A)))

(def (eval-A i j)
  (+ (vector-ref +A+ (+ i j)) i))

(defregister vi)
(def (times v u)
  (let ((vlen (f64vector-length v))
        (ulen (f64vector-length u)))
    (for (i (in-range vlen))
      (fl!= vi 0)
      (let loop ((j 0))
        (defrule (unroll i j x)
          (let* ((j+x (+ j x))
                 (a (eval-A i j+x)))
            (fl!= vi (+ vi (/ (@ u j+x) (->fl a))))))
        (when (< j ulen)
          (unroll i j 0)
          (unroll i j 1)
          (unroll i j 2)
          (unroll i j 3)
          (loop (+ j 4))))
      (fl!= (@ v i) vi))))

(def (times-transp v u)
  (let ((vlen (f64vector-length v))
        (ulen (f64vector-length u)))
    (for (i (in-range vlen))
      (fl!= vi 0)
      (let loop ((j 0))
        (defrule (unroll i j x)
          (let* ((j+x (+ j x))
                 (a (eval-A j+x i)))
            (fl!= vi (+ vi (/ (@ u j+x) (->fl a))))))
        (when (< j ulen)
          (unroll i j 0)
          (unroll i j 1)
          (unroll i j 2)
          (unroll i j 3)
          (loop (+ j 4))))
      (fl!= (@ v i) vi))))

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
    (precompute-A! n)
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
