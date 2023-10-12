;;; -*- Gerbil -*-
;;; © vyzo
;;; mandelbrot program from Computer Language Benchmarks Game
;;; ported from the racket versionk

(import :std/sugar
        :std/iter
        :std/format
        :std/text/utf8
        :std/os/fdio)
(export main)
(declare
  (not safe)
  (fixnum))

(include "io.ss")

(def (run id next)
  (let loop ()
    (let ((v (thread-receive)))
      (cond
       ((= v 0) ;; Done
        (write-output-string (number->string id))
        (write-output-newline)
        (flush-output)
        (exit))
       (else ;; Keep going
        (thread-send next (- v 1))
        (loop))))))

(def (main n)
  (let* ((n (string->number n))
         (t1 (for/fold (next (current-thread))
                 (id (in-range 502 0 -1))
               (thread-start!
                (make-thread
                 (lambda () (run id next)))))))
    ;; Start:
    (thread-send t1 n)
    (run 503 t1)))