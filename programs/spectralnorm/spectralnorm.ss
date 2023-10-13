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

(def +memo+ #f)
(def (memo i j)
  (let (i+j (+ i j))
    (cond
     ((vector-ref +memo+ i+j))
     (else
      (let (val (+ (// (* i+j (+ i+j 1)) 2) 1))
        (vector-set! +memo+ i+j val)
        val)))))

(def (eval-A i j)
  (+ (memo i j) i))

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
    (set! +memo+ (make-vector (* 2 n) #f))
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
