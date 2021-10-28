#lang racket

(require "utilities.rkt")

(provide shrink)

(define (shrink p)
  (match p
    [(ProgramDefsExp info def* e)
     (ProgramDefs info (append def* `(,(Def 'main '() 'Integer '() (shrink-exp e)))))]))

(define (shrink-exp e)
  (match e
    [(Void)         (Void)]
    [(Int i)        (Int i)]
    [(Bool b)       (Bool b)]
    [(Var v)        (Var v)]
    [(HasType e t)  (HasType (shrink-exp e) t)]
    [(Let v e b)    (Let v (shrink-exp e) (shrink-exp b))]
    [(If c t e)     (If (shrink-exp c) (shrink-exp t) (shrink-exp e))]
    [(Prim '- `(,e1 ,e2))
     (let ([t (gensym 'tmp)])
       (Let t (Prim '- `(,(shrink-exp e1))) (Prim '+ `(,(Var t) ,(shrink-exp e2)))))]
    [(Prim op es) #:when (member op '(read - + not < vector vector-length))
     (Prim op (map shrink-exp es))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (Prim 'vector-ref `(,(shrink-exp e) ,(Int i)))]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (Prim 'vector-set! `(,(shrink-exp e1) ,(Int i) ,(shrink-exp e2)))]
    [(Apply fun arg*)
     (Apply (shrink-exp fun) (map shrink-exp arg*))]
    [(Def name param* rty info e)
     (Def name param* rty info (shrink-exp e))]
    ; (and e1 e2) == (if e1 e2 #f)
    [(Prim 'and `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (If e1 e2 (Bool #f)))]
    ; (or e1 e1) == (if e1 #t e2)
    [(Prim 'or `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (If e1 (Bool #t) e2))]
    [(Prim 'eq? es) (Prim 'eq? (map shrink-exp es))]
    ; (>= e1 e2) == (not (< e1 e2))
    [(Prim '>= `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (Prim 'not `(,(Prim `< `(,e1 ,e2)))))]
    ; (<= e1 e2) == (let ([tmp e1]) (not (< e2 tmp)))
    ; tmp needed to enforce order of evaluation
    [(Prim '<= `(,e1 ,e2))
     (let ([v (gensym 'tmp)])
       (shrink-exp
         (Let v
              e1
              (Prim 'not
                    `(,(Prim '<
                             `(,e2 ,(Var v))))))))]
    ; (> e1 e2) == (not (<= e1 e2))
    [(Prim '> `(,e1 ,e2))
     (let ([e (Prim 'not `(,(Prim '<= `(,e1 ,e2))))])
       (shrink-exp e))]
    [(Lambda arg* rtype body)
     (Lambda arg* rtype (shrink-exp body))]))
