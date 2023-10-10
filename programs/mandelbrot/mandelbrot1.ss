;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from racket and heavily optimized for fp performance
(import :std/sugar
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

;; fp "registers"
(defregister rfp-ci)
(defregister rfp-cr)
(defregister rfp-zr)
(defregister rfp-zi)
(defregister tmp-zr)

(def (mandelbrot)
  (fl!= rfp-zr rfp-cr)
  (fl!= rfp-zi rfp-ci)
  (let loop ((i 1))
    (defrules unroll ()
      ((_ () continue)
       continue)
      ((_ (_ . rest) continue)
       (unroll1
         (unroll rest continue))))
    (defrule (unroll1 continue)
      (if (fl!? > (+ (^2 rfp-zr) (^2 rfp-zi)) +limit-sqr+)
        0
        (begin
          (fl!= tmp-zr rfp-zr)
          (fl!= rfp-zr (+ (- (^2 rfp-zr) (^2 rfp-zi)) rfp-cr))
          (fl!= rfp-zi (+ (* 2 tmp-zr rfp-zi) rfp-ci))
          continue)))
    (cond
     ((< (+ i 12) +iterations+)
      (unroll (o o o o o o o o o o o o)
        (loop (+ i 12))))
     ((<= i +iterations+)
      (unroll1 (loop (+ i 1))))
     (else 1))))

(defrules bits->byte ()
  ((_ bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0)
   (+ (* bit7 #b10000000)
      (* bit6 #b01000000)
      (* bit5 #b00100000)
      (* bit4 #b00010000)
      (* bit3 #b00001000)
      (* bit2 #b00000100)
      (* bit1 #b00000010)
      (* bit0 #b00000001))))

(def (mandelbrot-x y n)
  (fl!= rfp-cr -1.5)
  (let loop ((x 0) (bit 0) (byte 0))
    (defrules unroll* ()
      ((_ () continue ...)
       (begin continue ...))
      ((_ (bit . rest) continue ...)
       (unroll1 (bit)
         (unroll* rest continue ...))))
    (defrule (unroll1 (bitx) continue ...)
      (let (bitx (mandelbrot))
        (fl!= rfp-cr (+ rfp-cr (/ 2 (->fl n))))
        continue ...))
    (defrule (unroll (byte) continue ...)
      (unroll1 (m)
        (let (byte (+ (fxarithmetic-shift-left byte 1) m))
          continue ...)))
    (cond
     ((< (+ x 8) n)
      (unroll* (bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0)
        (write-output-u8 (bits->byte bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0))
        (loop (+ x 8) 0 0)))
     ((< x n)
      (unroll (byte)
        (cond
         ((= bit 7)
          (write-output-u8 byte)
          (loop (+ x 1) 0 0))
         (else
          (loop (+ x 1) (+ bit 1) byte)))))
     (else
      (when (> bit 0)
        (write-output-u8 (fxarithmetic-shift-left byte (- 8 (fxand n #x7)))))
      (mandelbrot-y (+ y 1) n)))))

(def (mandelbrot-y y n)
  (when (< y n)
    (fl!= rfp-ci (- (/ (* 2 (->fl y)) (->fl n)) 1))
    (mandelbrot-x y n)))

(def (mandelbrot-loop n)
  (mandelbrot-y 0 n))

(def (main n)
  (let (n (string->number n))
    (write-output-string (format "P4\n~a ~a\n" n n))
    (mandelbrot-loop n)
    (flush-output)))
