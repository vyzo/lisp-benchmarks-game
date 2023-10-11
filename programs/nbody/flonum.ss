;;; -*- Gerbil -*-
;;; © vyzo
;;; flonum zero-garbage macros

;; Note: eventually these macros can be moved to stdlib in Gerbil v0.19 in a fl=! dispatch macro,
;; which handles all the basic operations and fixnum mixing.
;; They are generally useful for working with flonums without creating intermediate garbage.
;; (yes, I am experimenting here)

(defrule (defregister name)
  (begin
    (def name (make-flonum))))

(def (make-flonum)
  (##c-code "___RESULT= ___F64BOX(0);"))

(defalias ->fl fixnum->flonum)
(defrules ^2 ())

(begin-syntax
  (def (collect-arguments stx)
    (deduplicate
     (let recur ((stx-expr stx) (ids []))
       (syntax-case stx-expr (^2 sqrt ->fl @)
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
         ((sqrt expr)
          (recur #'expr ids))
         ((->fl expr)
          (cond
           ((identifier? #'expr)
            (cons #'expr ids))
           ((stx-number? #'expr)
            ids)
           (else
            (raise-syntax-error #f "bad expression" stx stx-expr))))
         ((@ f64v offset)
          (and (identifier? #'f64v)
               (or (identifier? #'offset)
                   (stx-number? #'offset)))
          (cons #'f64v
                (if (identifier? #'offset)
                  (cons #'offset ids)
                  ids)))
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
    (syntax-case stx (+ - * / ^2 sqrt ->fl @)
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
      ((- expr)
       (string-append "-(" (expand-expr #'expr ids args) ")"))
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
      ((sqrt expr)
       (let (c-code (expand-expr #'expr ids args))
         (string-append "sqrt(" c-code ")")))
      ((->fl expr)
       (cond
        ((stx-datum? #'expr)
         (string-append "((double)" (number->string* (stx-e #'expr)) ")"))
        ((identifier? #'expr)
         (string-append "((double)___INT(" (identifier-argument #'expr ids args) "))"))
        (else
         (raise-syntax-error #f "bad expression" stx #'(->fl expr)))))
      ((@ f64v offset)
       (let ((f64v-arg (identifier-argument #'f64v ids args))
             (offset-arg (if (stx-number? #'offset)
                           (number->string (stx-e #'offset))
                           (identifier-argument #'offset ids args))))
         (string-append "___F64VECTORREF(" f64v-arg "," offset-arg ")")))))

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
  (syntax-case stx (@)
    ((_ reg expr)
     (identifier? #'reg)
     (with-syntax* (((id ...) (collect-arguments #'expr))
                    ((arg ...) (make-c-code-arguments #'(id ...) 2))
                    (c-code (expand-expr #'expr #'(id ...) #'(arg ...)))
                    (c-code (string-append "___F64UNBOX(___ARG1) = "
                                           (stx-e #'c-code)
                                           "; ___RESULT = ___VOID;")))
       #'(##c-code c-code reg id ...)))
    ((_ (@ f64v offset) expr)
     (and (identifier? #'f64v)
          (or (identifier? #'offset)
              (stx-number? #'offset)))
     (with-syntax* (((id ...) (collect-arguments #'expr))
                    ((arg ...) (make-c-code-arguments #'(id ...) 3))
                    (c-code (expand-expr #'expr #'(id ...) #'(arg ...)))
                    (c-code (string-append
                             "___F64VECTORSET(___ARG1, ___ARG2,"
                             (stx-e #'c-code)
                             "); ___RESULT = ___VOID;")))
       #'(##c-code c-code f64v offset id ...)))))

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

(defrules fl! ()
  ((_ 0) (make-flonum))
  ((_ expr)
   (let (reg (make-flonum))
     (fl!= reg expr)
     reg)))
