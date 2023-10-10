;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from racket and heavily optimized for fp performance
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

;; Note: eventually these macros can be moved to stdlib in Gerbil v0.19 in a fl=! dispatch macro,
;; which handles all the basic operations and fixnum mixing.
;; They are generally useful for working with flonums without creating intermediate garbage.
;; (yes, I am experimenting here)
(defalias ->fl fixnum->flonum)
(defrules fl+! (->fl)
  ((_ r (->fl x) (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = (___INT(___ARG2) + ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r (->fl x) y)
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) + ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) + ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x y)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) + ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y)))

(defrules fl-! (->fl)
  ((_ r (->fl x) (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) - ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r (->fl x) y)
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) - ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) - ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x y)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) - ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y)))

(defrules fl*! (->fl)
  ((_ r (->fl x) (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) * ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r (->fl x) y)
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) * ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) * ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x y)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x y (->fl z))
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3) * ___INT(___ARG4); ___RESULT = ___VOID;"
             r x y z)))

(defrules fl/! (->fl)
  ((_ r (->fl x) (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) / (double)___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r (->fl x) y)
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) / ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x (->fl y))
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) / ___INT(___ARG3); ___RESULT = ___VOID;"
             r x y))
  ((_ r x y)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) / ___F64UNBOX(___ARG3); ___RESULT = ___VOID;"
             r x y)))

(defrules fl/-! (->fl)
  ((_ r (->fl x) (->fl y) z)
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) / (double)___INT(___ARG3) - ___F64UNBOX(___ARG4); ___RESULT = ___VOID;"
             r x y z)))

(defrules fl*/-! (->fl)
  ((_ r (->fl x) (->fl y) (->fl z) (->fl w))
   (##c-code "___F64UNBOX(___ARG1) = ___INT(___ARG2) * ___INT(___ARG3) / (double)___INT(___ARG4) - ___INT(___ARG5); ___RESULT = ___VOID;"
             r x y z w)))

(defrules fl-+! ()
  ((_ r x y z)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2) - ___F64UNBOX(___ARG3) + ___F64UNBOX(___ARG4); ___RESULT= ___VOID;"
             r x y z)))

(defrules fl*2+! ()
  ((_ r x y z)
   (##c-code "___F64UNBOX(___ARG1) = 2 * ___F64UNBOX(___ARG2) * ___F64UNBOX(___ARG3)  + ___F64UNBOX(___ARG4); ___RESULT= ___VOID;"
             r x y z)))

(defrules fl=! ()
  ((_ r x)
   (##c-code "___F64UNBOX(___ARG1) = ___F64UNBOX(___ARG2); ___RESULT = ___VOID;"
             r x)))

(defrules fl^2! ()
  ((_ r x)
   (##c-code "___F64UNBOX(___ARG1) = ({double r2 = ___F64UNBOX(___ARG2); r2 *= r2; r2;}); ___RESULT= ___VOID;"
             r x)))

(defrules fl+^2>? ()
  ((_ x y z)
   (##c-code "double r1 = ___F64UNBOX(___ARG1); r1 *= r1; double r2 = ___F64UNBOX(___ARG2); r2 *= r2; if ((r1 + r2) > ___F64UNBOX(___ARG3)) { ___RESULT = ___TRU; } else { ___RESULT = ___FAL; }"
             x y z)))

(defrules fl-^2+! ()
  ((_ r x y z)
   (##c-code "___F64UNBOX(___ARG1) = ({double r =___F64UNBOX(___ARG2); r *= r; r;}) - ({double r = ___F64UNBOX(___ARG3); r *= r; r;}) + ___F64UNBOX(___ARG4); ___RESULT= ___VOID;"
             r x y z)))

(defrules fl*!+! (->fl)
  ((_ r x (->fl z) w)
   (##c-code "double r = ___F64UNBOX(___ARG1);  ___F64UNBOX(___ARG1) = r *___F64UNBOX(___ARG2) * ___INT(___ARG3) + ___F64UNBOX(___ARG4); ___RESULT = ___VOID;"
             r x z w)))

;; fp "registers"
(def rfp-ci   1337.0)
(def rfp-cr   1337.1)
(def rfp-zr   1337.2)
(def rfp-zi   1337.3)
(def tmp-zr   1337.4)
(def rfp-2/n  1337.5)

(def (mandelbrot)
  (fl=! rfp-zr rfp-cr)
  (fl=! rfp-zi rfp-ci)
  (let loop ((i 1))
    (defrules unroll ()
      ((_ () continue)
       continue)
      ((_ (_ . rest) continue)
       (unroll1
         (unroll rest continue))))
    (defrule (unroll1 continue)
      (if (fl+^2>? rfp-zr rfp-zi +limit-sqr+)
        0
        (begin
          (fl=! tmp-zr rfp-zr)
          (fl-^2+! rfp-zr rfp-zr rfp-zi rfp-cr)
          (fl*2+!  rfp-zi tmp-zr rfp-zi rfp-ci)
          continue)))
    (cond
     ((< (+ i 8) +iterations+)
      (unroll (o o o o o o o o)
        (loop (+ i 8))))
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
  (fl=! rfp-cr -1.5)
  (let loop ((x 0) (bit 0) (byte 0))
    (defrules unroll* ()
      ((_ () continue ...)
       (begin continue ...))
      ((_ (bit . rest) continue ...)
       (unroll1 (bit)
         (unroll* rest continue ...))))
    (defrule (unroll1 (bitx) continue ...)
      (let (bitx (mandelbrot))
        (fl+! rfp-cr rfp-cr rfp-2/n)
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
    (fl*/-! rfp-ci (->fl 2) (->fl y) (->fl n) (->fl 1))
    (mandelbrot-x y n)))

(def (mandelbrot-loop n)
  (fl/! rfp-2/n (->fl 2) (->fl n))
  (mandelbrot-y 0 n))

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
