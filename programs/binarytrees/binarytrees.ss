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

(defstruct node (left val right) final: #t)

(defrule (new item)
  (node #f item #f))

(defrule (leaf? l)
  (fixnum? l))

(def (make item d)
  (if (= d 0)
    item
    (let ((n (new item))
          (item2 (* item 2))
          (d2 (- d 1)))
        (make-left! (- item2 1) d2 n)
        (make-right! item2 d2 n)
        n)))

(defrule (defmake make! field)
  (with-id ((n 'n) (target #'n "." 'field))
    (def (make! item d n)
      (using (n :- node)
        (if (= d 0)
          (set! target item)
          (let ((nn (new item))
                (item2 (* item 2))
                (d2 (- d 1)))
            (set! target nn)
            (make-left! (- item2 1) d2 nn)
            (make-right! item2 d2 nn)))))))

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
              (check (make 0 stretch-depth))))
    (let (long-lived-tree (make 0 max-depth))
      (for (d (in-range 4 (+ max-depth 1) 2))
        (let ((iterations (<< 1 (+ (- max-depth d) min-depth))))
          (printf "~a\t trees of depth ~a\t check: ~a\n"
                  iterations
                  d
                  (for/fold (c 0) (i (in-range iterations))
                    (+ c (check (make i d)))))))
      (printf "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))
