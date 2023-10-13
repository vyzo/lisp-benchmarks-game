;;; -*- Gerbil -*-
;;; © vyzo
;;; binarytrees program from Computer Language Benchmarks Game

(import :std/contract
        :std/sugar
        :std/format
        :std/iter)
(export main)
(declare
  (not safe)
  (fixnum))
(include "int.ss")

(defalias node cons)
(defrule (new)
  (node #f #f))

(defrule (leaf? l)
  (not l))

(def (make d)
  (if (= d 0)
    #f
    (let ((n (new))
          (d2 (- d 1)))
        (make-left! d2 n)
        (make-right! d2 n)
        n)))

(defrule (defmake make! target)
  (def (make!  d n)
    (if (= d 0)
      (set! (target n) #f)
      (let ((nn (new))
            (d2 (- d 1)))
        (set! (target n) nn)
        (make-left! d2 nn)
        (make-right! d2 nn)))))

(defmake make-left! car)
(defmake make-right! cdr)

(def (check t)
  (if (leaf? t)
    1
    (+ 1 (+ (check (car t))
            (check (cdr t))))))

(def (main n)
  (let* ((n (string->number n))
         (min-depth 4)
         (max-depth (max (+ min-depth 2) n)))
    (let (stretch-depth (+ max-depth 1))
      (printf "stretch tree of depth ~a\t check: ~a\n"
              stretch-depth
              (check (make stretch-depth))))
    (let (long-lived-tree (make max-depth))
      (for (d (in-range 4 (+ max-depth 1) 2))
        (let ((iterations (<< 1 (+ (- max-depth d) min-depth))))
          (printf "~a\t trees of depth ~a\t check: ~a\n"
                  iterations
                  d
                  (for/fold (c 0) (_ (in-range iterations))
                    (+ c (check (make d)))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))
