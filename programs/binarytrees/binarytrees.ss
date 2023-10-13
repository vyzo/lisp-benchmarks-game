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

(defstruct node (left right) final: #t)

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

(defrule (defmake make! field)
  (with-id ((n 'n) (target #'n "." 'field))
    (def (make!  d n)
      (using (n :- node)
        (if (= d 0)
          (set! target #f)
          (let ((nn (new))
                (d2 (- d 1)))
            (set! target nn)
            (make-left! d2 nn)
            (make-right! d2 nn)))))))

(defmake make-left! left)
(defmake make-right! right)

(def (check t)
  (if (leaf? t)
    1
    (using (t :- node)
      (+ 1 (+ (check t.left)
              (check t.right))))))

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
