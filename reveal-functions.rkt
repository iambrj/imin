#lang racket

(require "utilities.rkt")

(provide reveal-functions)

; Transform function references from just f to (FunRefArity f arity)
(define (reveal-functions p)
  (match p
    [(ProgramDefs info def*)
     (let ([fun->arity (for/list ([def def*])
                         (cons (Def-name def) (length (Def-param* def))))])
       (ProgramDefs info (map (lambda (d)
                                (struct-copy Def d
                                             [body ((rf-exp fun->arity) (Def-body d))]))
                              def*)))]))

(define ((rf-exp fun->arity) e)
  (match e
    [(Void)         (Void)]
    [(Int i)        (Int i)]
    [(Bool b)       (Bool b)]
    [(Var v)        (if (member v (dict-keys fun->arity))
                      (FunRefArity v (dict-ref fun->arity v))
                      (Var v))]
    [(HasType e t)  (HasType ((rf-exp fun->arity) e) t)]
    [(Let v e b)    (Let v ((rf-exp fun->arity) e)
                         ((rf-exp (dict-remove fun->arity v)) b))]
    [(If c t e)     (If ((rf-exp fun->arity) c)
                        ((rf-exp fun->arity) t)
                        ((rf-exp fun->arity) e))]
    [(Prim op es) #:when (member op '(read - + not < vector vector-length
                                           procedure-arity))
     (Prim op (map (rf-exp fun->arity) es))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (Prim 'vector-ref `(,((rf-exp fun->arity) e) ,(Int i)))]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (Prim 'vector-set! `(,((rf-exp fun->arity) e1)
                           ,(Int i) ,((rf-exp fun->arity) e2)))]
    ; Invariant : only top level function definitions are allowed, no clause
    ; needed for Def
    [(Apply fun arg*)
     (Apply ((rf-exp fun->arity) fun) (map (rf-exp fun->arity) arg*))]
    [(Lambda arg* rtype body)
     (let ([fun->arity (foldr (lambda (arg a)
                                (dict-remove a arg))
                              fun->arity
                              arg*)])
      (Lambda arg* rtype ((rf-exp fun->arity) body)))]))
