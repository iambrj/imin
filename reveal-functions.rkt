#lang racket

(require "utilities.rkt")

(provide reveal-functions)

; Transform function references from just f to (FunRef f)
(define (reveal-functions p)
  (match p
    [(ProgramDefs info def*)
     (let ([funs (map Def-name def*)])
       (ProgramDefs info (map (lambda (d)
                                (struct-copy Def d
                                             [body ((rf-exp funs) (Def-body d))]))
                              def*)))]))

(define ((rf-exp funs) e)
  (match e
    [(Void)         (Void)]
    [(Int i)        (Int i)]
    [(Bool b)       (Bool b)]
    [(Var v)        (if (member v funs) (FunRef v) (Var v))]
    [(HasType e t)  (HasType ((rf-exp funs) e) t)]
    [(Let v e b)    (Let v ((rf-exp funs) e) (rf-exp (remove v funs) b))]
    [(If c t e)     (If ((rf-exp funs) c) ((rf-exp funs) t) ((rf-exp funs) e))]
    [(Prim op es) #:when (member op '(read - + not < vector vector-length))
     (Prim op (map (rf-exp funs) es))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (Prim 'vector-ref `(,((rf-exp funs) e) ,(Int i)))]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (Prim 'vector-set! `(,((rf-exp funs) e1) ,(Int i) ,((rf-exp funs) e2)))]
    ; Invariant : only top level function definitions are allowed, no clause
    ; needed for Def
    [(Apply fun arg*)
     (Apply ((rf-exp funs) fun) (map (rf-exp funs) arg*))]))
