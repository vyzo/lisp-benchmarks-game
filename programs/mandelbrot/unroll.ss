;;; -*- Gerbil -*-
;;; © vyzo
;;; general unrolling macros

(defsyntax (unroll stx)
  (def (generate times var expr)
    (if (= times 1)
      expr
      (with-syntax ((var var) (expr expr) (rest (generate (- times 1) var expr)))
        #'(let (var (+ var 1)) expr rest))))
  (syntax-case stx ()
    ((_ times var expr)
     (stx-number? #'times)
     (with-syntax ((rest (generate (stx-e #'times)  #'var  #'expr)))
       #'(let (var 0) expr rest)))))
