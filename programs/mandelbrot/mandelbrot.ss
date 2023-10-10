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

(def +limit-sqr+ 4.0)
(def +iterations+ 50)

;; Note: eventually these macros can be moved to stdlib in Gerbil v0.19 in a fl=! dispatch macro,
;; which handles all the basic operations and fixnum mixing.
;; They are generally useful for working with flonums without creating intermediate garbage.
;; (yes, I am experimenting here)

(defrule (defregister name)
  (begin
    (def name (fixnum->flonum (random-integer 1337)))))

(defalias ->fl fixnum->flonum)
(defrules ^2 ())

(begin-syntax
  (def (collect-arguments stx)
    (deduplicate
     (let recur ((stx-expr stx) (ids []))
       (syntax-case stx-expr (^2 ->fl)
         (id (identifier? #'id) (cons #'id ids))
         (datum (stx-number? #'datum) ids)
         ((op expr ...)
          (and (identifier? #'op)
               (or (free-identifier=? #'op #'+)
                   (free-identifier=? #'op #'-)
                   (free-identifier=? #'op #'*)
                   (free-identifier=? #'op #'/)
                   (free-identifier=? #'op #'?)))
          (let lp ((rest #'(expr ...)) (ids ids))
            (syntax-case rest ()
              ((hd . rest)
               (lp #'rest (recur #'hd ids)))
              (() ids))))
         ((^2 expr)
          (recur #'expr ids))
         ((->fl expr)
          (cond
           ((identifier? #'expr)
            (cons #'expr ids))
           ((stx-number? #'expr)
            ids)
           (else
            (raise-syntax-error #f "bad expression" stx stx-expr))))
         (_
          (raise-syntax-error #f "bad expression" stx stx-expr))))))

  (def (deduplicate ids)
    (let lp ((rest ids) (result []))
      (match rest
        ([id . rest]
         (if (find (cut bound-identifier=? <> id) result)
           (lp rest result)
           (lp rest (cons id result))))
        (else result))))

  (def (make-c-code-arguments ids next)
    (let lp ((rest ids) (args []) (next next))
      (match rest
        ([_ . rest]
         (lp rest (cons (string-append "___ARG" (number->string next)) args) (+ next 1)))
        ([] (reverse args)))))

  (def (expand-expr stx ids args)
    (syntax-case stx (+ - * / ^2 ->fl)
      (id (identifier? #'id)
          (let (arg (identifier-argument #'id ids args))
            (string-append "___F64UNBOX(" arg ")")))
      (datum (stx-number? #'datum)
             (if (negative? (stx-e #'datum))
               (string-append "(" (number->string* (stx-e #'datum)) ")")
               (number->string* (stx-e #'datum))))
      ((+ expr ...)
       (string-append "("
                      (string-join (stx-map (cut expand-expr <> ids args) #'(expr ...))
                                   #\+)
                      ")"))

      ((- expr ...)
       (string-append "("
                      (string-join (stx-map (cut expand-expr <> ids args) #'(expr ...))
                                   #\-)
                      ")"))
      ((* expr ...)
       (string-append "("
                      (string-join (stx-map (cut expand-expr <> ids args) #'(expr ...))
                                   #\*)
                      ")"))
      ((/ expr ...)
       (string-append "("
                      (string-join (stx-map (cut expand-expr <> ids args) #'(expr ...))
                                   #\/)
                      ")"))
      ((^2 expr)
       (let (c-code (expand-expr #'expr ids args))
         (string-append "({double r = " c-code "; r *= r; r;})")))
      ((->fl expr)
       (cond
        ((stx-datum? #'expr)
         (string-append "((double)" (number->string* (stx-e #'expr)) ")"))
        ((identifier? #'expr)
         (string-append "((double)___INT(" (identifier-argument #'expr ids args) "))"))
        (else
         (raise-syntax-error #f "bad expression" stx #'(->fl expr)))))))

  (def (identifier-argument id ids args)
    (let loop ((rest-ids ids) (rest-args args))
      (match rest-ids
        ([xid . rest-ids]
         (if (bound-identifier=? id xid)
           (car rest-args)
           (loop rest-ids (cdr rest-args)))))))

  (def (number->string* datum)
    (let (str (number->string datum))
      (cond
       ((string-prefix? "." str)
        (string-append "0" str))
       ((string-prefix? "-." str)
        (string-append "-0." (substring str 2 (string-length str))))
       ((eqv? (string-ref str (1- (string-length str))) #\.)
        (string-append str "0"))
       (else str)))))

(defsyntax (fl!= stx)
  (syntax-case stx ()
    ((_ reg expr)
     (identifier? #'reg)
     (with-syntax* (((id ...) (collect-arguments #'expr))
                    ((arg ...) (make-c-code-arguments #'(id ...) 2))
                    (c-code (expand-expr #'expr #'(id ...) #'(arg ...)))
                    (c-code (string-append "___F64UNBOX(___ARG1) = "
                                           (stx-e #'c-code)
                                           "; ___RESULT = ___VOID;")))
       #'(##c-code c-code reg id ...)))))

(defsyntax (fl!? stx)
  (syntax-case stx ()
    ((_ op e1 e2)
     (and (identifier? #'op)
          (or (free-identifier=? #'op #'>)
              (free-identifier=? #'op #'>=)
              (free-identifier=? #'op #'=)
              (free-identifier=? #'op #'<=)
              (free-identifier=? #'op #'<)))
     (with-syntax* (((id ...) (collect-arguments #'(? e1 e2)))
                    ((arg ...) (make-c-code-arguments #'(id ...) 1))
                    (c-code1 (expand-expr #'e1 #'(id ...) #'(arg ...)))
                    (c-code2 (expand-expr #'e2 #'(id ...) #'(arg ...)))
                    (c-op (if (free-identifier=? #'op #'=)
                            "=="
                            (symbol->string (stx-e #'op))))
                    (c-code
                     (string-append "___RESULT = (" (stx-e #'c-code1) (stx-e #'c-op) (stx-e #'c-code2)
                                    ") ? ___TRU : ___FAL;")))
       #'(##c-code c-code id ...)))))

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
