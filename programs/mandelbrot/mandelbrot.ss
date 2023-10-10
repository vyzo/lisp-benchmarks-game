;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from racket and optimized for fp performance
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

;; Note: eventually these macros can be moved to stdlib in Gerbil v0.19 in a fl! dispatch macro.
;; they are generally useful for working with flonums without creating intermediate garbage.
(defalias ->fl fixnum->flonum)
(defrules fl+! (->fl)
  ((_ scratch (->fl x) (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = (___INT(___ARG2) + ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch (->fl x) y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) + ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) + ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) + ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y)))

(defrules fl-! (->fl)
  ((_ scratch (->fl x) (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) - ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch (->fl x) y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) - ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) - ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) - ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y)))

(defrules fl*! (->fl)
  ((_ scratch (->fl x) (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) * ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch (->fl x) y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) * ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) * ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x y (->fl z))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3) * ___INT(___ARG4); ___RESULT= ___ARG1;"
             scratch x y z)))

(defrules fl/! (->fl)
  ((_ scratch (->fl x) (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) / (double)___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch (->fl x) y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) / ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x (->fl y))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) / ___INT(___ARG3); ___RESULT= ___ARG1;"
             scratch x y))
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) / ___F64UNBOX(___ARG3); ___RESULT= ___ARG1;"
             scratch x y)))

(defrules fl+^2! ()
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = pow(___F64UNBOX(___ARG2), 2) + pow(___F64UNBOX(___ARG3), 2); ___RESULT= ___ARG1;"
             scratch x y)))

(defrules fl-^2! ()
  ((_ scratch x y)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = pow(___F64UNBOX(___ARG2), 2) - pow(___F64UNBOX(___ARG3), 2); ___RESULT= ___ARG1;"
             scratch x y)))

(defrules fl-^2+! ()
  ((_ scratch x y z)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ({double r = ___F64UNBOX(___ARG2); r *= r; r;}) - ({double r = ___F64UNBOX(___ARG3); r *= r; r;}) + ___F64UNBOX(___ARG4); ___RESULT= ___ARG1;"
             scratch x y z)))

(defrules fl/-! (->fl)
  ((_ scratch (->fl x) (->fl y) z)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) / (double)___INT(___ARG3) - ___F64UNBOX(___ARG4); ___RESULT= ___ARG1;"
             scratch x y z)))

(defrules fl*/! (->fl)
  ((_ scratch (->fl x) (->fl y) (->fl z))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) * ___INT(___ARG3) / (double)___INT(___ARG4); ___RESULT= ___ARG1;"
             scratch x y z)))

(defrules fl*/-! (->fl)
  ((_ scratch (->fl x) (->fl y) (->fl z) (->fl w))
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___INT(___ARG2) * ___INT(___ARG3) / (double)___INT(___ARG4) - ___INT(___ARG5); ___RESULT= ___ARG1;"
             scratch x y z w)))

(defrules fl*+! (->fl)
  ((_ scratch x y (->fl z) w)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3) * ___INT(___ARG4) + ___F64UNBOX(___ARG5); ___RESULT= ___ARG1;"
             scratch x y z w)))

(defrules fl=! ()
  ((_ scratch x)
   (##c-code "double *r = ___CAST(___F64*,___BODY_AS(___ARG1,___tSUBTYPED)); *r = ___F64UNBOX(___ARG2); ___RESULT= ___ARG1;"
             scratch x)))

(defrules fl+^2>? ()
  ((_ x y z)
   (##c-code "___RESULT = ((({double r = ___F64UNBOX(___ARG1); r *= r; r;}) + ({double r = ___F64UNBOX(___ARG2); r *= r; r;})) > ___F64UNBOX(___ARG3)) ? ___TRU : ___FAL; "
             x y z)))

;; fp "registers"
(def rfp-ci 1337.0)
(def rfp-cr 1337.1)
(def rfp-zr 1337.2)
(def rfp-zi 1337.3)
(def tmp1   1337.4)
(def tmp2   1337.5)
(def tmp3   1337.6)

(def (mandelbrot x n)
  (let (t1 (* 2 x))
    (fl/-! rfp-cr (->fl t1) (->fl n) 1.5))
  (fl=! rfp-zr 0.0)
  (fl=! rfp-zi 0.0)
  (let loop ((i 0))
    (cond
     ((> i +iterations+)
      1)
     ((fl+^2>? rfp-zr rfp-zi +limit-sqr+)
      0)
     (else
      (fl=! tmp1 rfp-zr)
      (fl-^2+! rfp-zr tmp1 rfp-zi rfp-cr)
      (fl*+! rfp-zi tmp1 rfp-zi (->fl 2) rfp-ci)
      (loop (+ 1 i))))))

(def (mandelbrot-loop n)
  (let loop-y ((y 0))
    (when (< y n)
      (fl*/-! rfp-ci (->fl 2) (->fl y) (->fl n) (->fl 1))
      (let loop-x ((x 0) (bitnum 0) (byteacc 0))
        (if (< x n)
          (let ((bitnum (+ 1 bitnum))
                (byteacc (+ (fxarithmetic-shift-left byteacc 1)
                            (mandelbrot x n))))
            (cond
             ((= bitnum 8)
              (write-output-u8 byteacc)
              (loop-x (+ 1 x) 0 0))
             (else
              (loop-x (+ 1 x) bitnum byteacc))))
          (begin
            (when (> bitnum 0)
              (write-output-u8 (fxarithmetic-shift byteacc (- 8 (fxand n #x7)))))
            (loop-y (+ y 1))))))))

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
