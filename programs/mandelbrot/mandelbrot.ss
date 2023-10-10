;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from racket version
(import :std/format)
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
           ((fl> (fl+ (fl* zr zr) (fl* zi zi)) +limit-sqr+) 0)
           (else (loop (+ 1 i)
                       (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr)
                       (fl+ (fl* 2.0 (fl* zr zi)) ci))))))))

(def (main n)
  (let ((n (string->number n))
        (out (current-output-port)))
    (fprintf out "P4\n~a ~a\n" n n)
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
                    (write-u8 byteacc out)
                    (loop-x (+ 1 x) 0 0))
                   (else (loop-x (+ 1 x) bitnum byteacc))))
                (begin
                  (when (positive? bitnum)
                    (write-u8 (fxarithmetic-shift byteacc (- 8 (fxand n #x7)))
                              out))
                  (loop-y (+ y 1))))))))))
