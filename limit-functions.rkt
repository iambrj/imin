#lang racket

(require "utilities.rkt")

(provide limit-functions)

(define (limit-functions p)
  (match p
    [(ProgramDefs info def*)
     (let ([def* (map limit-def def*)])
       (ProgramDefs info (map (lambda (d)
                                (struct-copy Def d
                                             [body (limit-calls (Def-body d))]))
                              def*)))]))

(define (limit-def d)
  (match d
    [(Def name param* rty info body)
     (let* ([vec (gensym 'arg6-)]
            [types (map caddr param*)]
            [vectorized (if (> (length param*) 6)
                          (map car (drop param* 5))
                          '())]
            [param* (if (> (length param*) 6)
                     (append (take param* 5)
                             `((,vec : ,(cons 'Vector (drop types 5)))))
                     param*)])
       (Def name param* rty info ((update-body vec vectorized) body)))]))

(define ((update-body vec vectorized) body)
  (if (not (null? vectorized))
    (match body
      [(Void)         (Void)]
      [(Int i)        (Int i)]
      [(Bool b)       (Bool b)]
      [(Var v)        (if (member v vectorized)
                        (Prim 'vector-ref `(,(Var vec) ,(Int (index-of vectorized v))))
                        (Var v))]
      [(HasType e t)  (HasType ((update-body vec vectorized) e) t)]
      [(If c t e)     (If ((update-body vec vectorized) c)
                          ((update-body vec vectorized) t)
                          ((update-body vec vectorized) e))]
      [(Let v e b)
       ; XXX : Trade off between this trick and deleteing elements?
       (let ([vectorized1 (if (member v vectorized)
                            (list-update vectorized
                                         (index-of vectorized v)
                                         (gensym))
                            vectorized)])
         (Let v ((update-body vec vectorized) e)
              ((update-body vec vectorized1) b)))]
      [(Prim op es) #:when (member op '(read - + not < vector vector-length))
       (Prim op (map (update-body vec vectorized) es))]
      [(Prim 'vector-ref `(,e ,(Int i)))
       (Prim 'vector-ref `(,((update-body vec vectorized) e) ,(Int i)))]
      [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
       (Prim 'vector-set! `(,((update-body vec vectorized) e1)
                             ,(Int i)
                             ,((update-body vec vectorized) e2)))]
      ; Invariant : only top level function definitions are allowed, no clause
      ; needed for Def
      [(Apply fun arg*)
       (Apply ((update-body vec vectorized) fun) (map (update-body vec vectorized) arg*))])
    body))

(define (limit-calls e)
  (match e
      [(Void)         (Void)]
      [(Int i)        (Int i)]
      [(Bool b)       (Bool b)]
      [(Var v)        (Var v)]
      [(FunRef f)     (FunRef f)]
      [(HasType e t)  (HasType (limit-calls e) t)]
      [(If c t e)     (If (limit-calls c)
                          (limit-calls t)
                          (limit-calls e))]
      [(Let v e b)
       ; XXX : Trade off between this trick and deleteing elements?
       (Let v (limit-calls e) (limit-calls b))]
      [(Prim op es) #:when (member op '(read - + not < vector vector-length))
       (Prim op (map limit-calls es))]
      [(Prim 'vector-ref `(,e ,(Int i)))
       (Prim 'vector-ref `(,(limit-calls e) ,(Int i)))]
      [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
       (Prim 'vector-set! `(,(limit-calls e1)
                             ,(Int i)
                             ,(limit-calls e2)))]
      ; Invariant : only top level function definitions are allowed, no clause
      ; needed for Def
      [(Apply fun arg*)
       (Apply (limit-calls fun)
              (let ([arg* (if (> (length arg*) 6)
                            (append (take arg* 5)
                             `(,(Prim 'vector (drop arg* 5))))
                            arg*)])
                (map limit-calls arg*)))]))
