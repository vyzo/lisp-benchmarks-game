;;; -*- Gerbil -*-
;;; © vyzo
;;; fannkuchredux program from Computer Language Benchmarks Game
;;; ported and tweaked from the racket ersion

(import :std/sugar
        :std/iter
        :std/format)
(export main)
(declare
  (not safe)
  (fixnum))

(def (fannkuch n)
  (let ((pi (list->vector (iota n)))
        (tmp (make-vector n))
        (count (make-vector n)))
    (let loop ((flips 0)
               (perms 0)
               (r n)
               (checksum 0)
               (even-parity? #t))
      (for (i (in-range r))
        (vector-set! count i (+ 1 i)))
      (let* ((next-flips (count-flips pi tmp))
             (flips2 (max next-flips flips))
             (next-checksum (+ checksum (if even-parity? next-flips (- 0 next-flips)))))
        (let loop2 ((r 1))
          (if (= r n)
            (values flips2 next-checksum)
            (let (perm0 (vector-ref pi 0))
              (for (i (in-range r))
                (vector-set! pi i (vector-ref pi (+ 1 i))))
              (vector-set! pi r perm0)
              (vector-set! count r (- (vector-ref count r) 1))
              (cond
               ((<= (vector-ref count r) 0)
                (loop2 (+ 1 r)))
               (else
                (loop flips2
                      (+ 1 perms)
                      r
                      next-checksum
                      (not even-parity?)))))))))))

(def (count-flips pi rho)
  (for (i (in-range (vector-length pi)))
    (vector-set! rho i (vector-ref pi i)))
  (let loop ((i 0))
    (if (= (vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (+ 1 (vector-ref rho 0)))
          (loop (+ 1 i))))))

(def (vector-reverse-slice! v i j)
  (let loop ((i i)
             (j (- j 1)))
    (when (> j i)
      (vector-swap! v i j)
      (loop (+ 1 i) (- j 1)))))

(defrule (vector-swap! v i j)
  (let (t (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(def (main n)
  (let ((values answer checksum)
        (fannkuch (string->number n)))
    (printf "~a\nPfannkuchen(~a) = ~a\n"
            checksum
            n
            answer)))
