;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from the Go version
;;; see mandelbrot1.ss for a version port4ed from racket
(import :std/sugar
        :std/iter
        :std/format
        :std/text/utf8
        :std/os/fdio
        :gerbil/gambit)
(export main)
(declare
  (not safe)
  (fixnum))

(include "flonum.ss")
(include "io.ss")
(include "int.ss")

(def +limit-sqr+ 4.0)
(def +iterations+ 50)

(def initial-r #f)
(def initial-i #f)
(def pixels #f)

(defregister prefetched-initial-i)
(defregister reg-r)
(defregister reg-i)

(def (calc image-width-and-height)
  (let ((pixel-group-r (make-f64vector 8))
        (pixel-group-i (make-f64vector 8)))
    (for (y (in-range image-width-and-height))
      (fl!= prefetched-initial-i (@ initial-i y))
      (for (x-major (in-range 0 image-width-and-height 8))
        (for (x-minor (in-range 8))
          (let (x-major+minor (+ x-major x-minor))
            (fl!= (@ pixel-group-r x-minor) (@ initial-r x-major+minor))
            (fl!= (@ pixel-group-i x-minor) prefetched-initial-i)))
        (let loop ((iteration +iterations+) (eight-pixels #xff))
          (if (or (= eight-pixels 0) (= iteration 0))
            (u8vector-set! pixels (+ (// (* y image-width-and-height) 8) (// x-major 8))
                           eight-pixels)
            (let loop-x ((x-minor 0) (eight-pixels eight-pixels) (current-pixel-bitmask #x80))
              (if (< x-minor 8)
                (let (x-major+minor (+ x-major x-minor))
                  (fl!= reg-r (@ pixel-group-r x-minor))
                  (fl!= reg-i (@ pixel-group-i x-minor))
                  (fl!= (@ pixel-group-r x-minor)
                        (+ (- (^2 reg-r) (^2 reg-i)) (@ initial-r x-major+minor)))
                  (fl!= (@ pixel-group-i x-minor)
                        (+ (* 2 reg-r reg-i) prefetched-initial-i))
                  (loop-x (+ x-minor 1)
                          (if (fl!? > (+ (^2 reg-r) (^2 reg-i)) +limit-sqr+)
                            (fxand eight-pixels (fxnot current-pixel-bitmask))
                            eight-pixels)
                          (>> current-pixel-bitmask 1)))
                (loop (- iteration 1) eight-pixels)))))))))

(def (main n)
  (let* ((n (string->number n))
         (image-width-and-height (* (// (+ n 7) 8) 8)))
    (set! pixels (make-u8vector (// (expt image-width-and-height 2) 8) 0))
    (set! initial-r (make-f64vector image-width-and-height))
    (set! initial-i (make-f64vector image-width-and-height))
    (for (xy (in-range image-width-and-height))
      (fl!= (@ initial-r xy) (- (* (/ 2 (->fl image-width-and-height)) (->fl xy)) 1.5))
      (fl!= (@ initial-i xy) (- (* (/ 2 (->fl image-width-and-height)) (->fl xy)) 1.0)))
    (calc image-width-and-height)
    (write-output-string (format "P4\n~a ~a\n" n n))
    (write-output-u8vector pixels)))
