;;; -*- Gerbil -*-
;;; © vyzo
;;; common IO utilities for writing directly to stdout, bypassing the io stack

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

(defrule (write-output-u8vector bytes)
  (begin
    (flush-output)
    (fdwrite +output-fd+ bytes)))

(defrule (write-output-newline)
  (write-output-char #\newline))

(defrule (write-output-char char)
  (write-output-u8 (char->integer char)))

(defrule (flush-output)
  (when (> +output-cursor+ 0)
    (fdwrite +output-fd+ +output-buffer+ 0 +output-cursor+)
    (set! +output-cursor+ 0)))
