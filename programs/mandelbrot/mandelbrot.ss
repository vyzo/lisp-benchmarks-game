;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from racket version
(import :std/sugar
        :std/format
        :std/text/utf8
        :std/os/fdio)
(export main)
(declare
  (not safe)
  (fixnum))

(def +limit-sqr+ 4.0)
(def +iterations+ 50)

(defalias ->fl fixnum->flonum)

(def (mandelbrot x y n ci)
  (let ((cr (fl- (fl/ (fl* 2.0 (->fl x)) (->fl n)) 1.5)))
    (let loop ((i 0) (zr 0.0) (zi 0.0))
      (if (> i +iterations+)
          1
          (cond
           ((fl> (fl+ (fl* zr zr) (fl* zi zi)) +limit-sqr+)
            0)
           (else
            (loop (+ 1 i)
                  (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr)
                  (fl+ (fl* 2.0 (fl* zr zi)) ci))))))))

(def (mandelbrot-loop n)
  (let loop-y ((y 0))
    (when (< y n)
      (let ((ci (fl- (fl/ (fl* 2.0 (->fl y)) (->fl n)) 1.0)))
        (let loop-x ((x 0) (bitnum 0) (byteacc 0))
          (if (< x n)
            (let ((bitnum (+ 1 bitnum))
                  (byteacc (+ (fxarithmetic-shift-left byteacc 1)
                              (mandelbrot x y n ci))))
              (cond
               ((= bitnum 8)
                (write-output-u8 byteacc)
                (loop-x (+ 1 x) 0 0))
               (else (loop-x (+ 1 x) bitnum byteacc))))
            (begin
              (when (positive? bitnum)
                (write-output-u8 (fxarithmetic-shift byteacc (- 8 (fxand n #x7)))))
              (loop-y (+ y 1)))))))))

(def (main n)
  (let (n (string->number n))
    (write-output-string (format "P4\n~a ~a\n" n n))
    (mandelbrot-loop n)
    (flush-output)))

;;; Common IO for the benchmarks; avoid ports like the plauge!
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
