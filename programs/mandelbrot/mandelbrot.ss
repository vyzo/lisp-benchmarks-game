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

(def +limit-sqr+ 4.0)
(def +iterations+ 50)

(def initial-r #f)
(def initial-i #f)
(def pixels #f)

(defregister prefetched-initial-i)
(defregister reg-r)
(defregister reg-i)

(def (calc image-width-and-height)
  (let ((y 0)
        (x-major 0)
        (pixel-group-r (make-f64vector 8 0.0))
        (pixel-group-i (make-f64vector 8 0.0))
        (eight-pixels #xff)
        (iteration +iterations+))
    (while (< y image-width-and-height)
      (fl!= prefetched-initial-i (@ initial-i y))
      (set! x-major 0)
      (while (< x-major image-width-and-height)
        (for (x-minor (in-range 8))
          (let (x-major+minor (+ x-major x-minor))
            (fl!= (@ pixel-group-r x-minor) (@ initial-r x-major+minor))
            (fl!= (@ pixel-group-i x-minor) prefetched-initial-i)))
        (set! eight-pixels #xff)
        (set! iteration +iterations+)
        (until (or (= eight-pixels 0) (= iteration 0))
          (let (current-pixel-bitmask #x80)
            (for (x-minor (in-range 8))
              (let (x-major+minor (+ x-major x-minor))
                (fl!= reg-r (@ pixel-group-r x-minor))
                (fl!= reg-i (@ pixel-group-i x-minor))
                (fl!= (@ pixel-group-r x-minor)
                      (+ (- (^2 reg-r) (^2 reg-i)) (@ initial-r x-major+minor)))
                (fl!= (@ pixel-group-i x-minor)
                      (+ (* 2 reg-r reg-i) prefetched-initial-i))
                (when (fl!? > (+ (^2 reg-r) (^2 reg-i)) +limit-sqr+)
                  (set! eight-pixels
                    (fxand eight-pixels (fxnot current-pixel-bitmask))))
                (set! current-pixel-bitmask
                  (fxarithmetic-shift-right current-pixel-bitmask 1)))))
          (set! iteration (- iteration 1)))
        (u8vector-set! pixels (+ (quotient (* y image-width-and-height) 8) (quotient x-major 8))
                       eight-pixels)
        (set! x-major (+ x-major 8)))
      (set! y (+ y 1)))))

(def (main n)
  (let* ((n (string->number n))
         (image-width-and-height (* (quotient (+ n 7) 8) 8)))
    (set! pixels (make-u8vector (quotient (expt image-width-and-height 2) 8) 0))
    (set! initial-r (make-f64vector image-width-and-height))
    (set! initial-i (make-f64vector image-width-and-height))
    (for (xy (in-range image-width-and-height))
      (fl!= (@ initial-r xy) (- (* (/ 2 (->fl image-width-and-height)) (->fl xy)) 1.5))
      (fl!= (@ initial-i xy) (- (* (/ 2 (->fl image-width-and-height)) (->fl xy)) 1.0)))
    (calc image-width-and-height)
    (write-output-string (format "P4\n~a ~a\n" n n))
    (write-output-u8vector pixels)))
