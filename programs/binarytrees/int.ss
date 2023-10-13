;;; -*- Gerbil -*-
;;; © vyzo
;;; some useful aliases for prettier code

(defrule (>> x i)
  (fxarithmetic-shift-right x i))
(defrule (<< x i)
  (fxarithmetic-shift-left x i))
(defrule (// x y)
  (quotient x y))
(defrule (% x y)
  (remainder x y))
