;;; © vyzo
;;; utility macro for multi-dimentional vectors
(defrules @@ ()
  ((_ v i)
   (vector-ref v i))
  ((_ v i rest ...)
   (@@ (vector-ref v i) rest ...)))

(defrules @@-set! ()
  ((_ v i val)
   (vector-set! v i val))
  ((_ v i j rest ...)
   (@@-set! (vector-ref v i) j rest ...)))

(defrules make-vector* ()
  ((_ i iv)
   (make-vector i iv))
  ((_ i j rest ...)
   (let (v (make-vector i))
     (for (x (in-range i))
       (vector-set! v x (make-vector* j rest ...)))
     v)))
