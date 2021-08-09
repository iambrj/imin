#lang racket

(require "utilities.rkt"
         "utils.rkt")

(provide uniquify)

(define ((uniquify-exp env) e)
  (match e
    [(Void)           (Void)]
    [(Var x)          (Var (dict-ref env x))]
    [(Int n)          (Int n)]
    [(Bool b)         (Bool b)]
    [(FunRef f)       (FunRef f)]
    [(GlobalValue g)  (GlobalValue g)]
    [(Allocate l t)   (Allocate l t)]
    [(Collect bytes)  (Collect bytes)]
    [(Apply fun arg*) (Apply ((uniquify-exp env) fun)
                             (map (uniquify-exp env) arg*))]
    [(HasType e t)    (HasType ((uniquify-exp env) e) t)]
    [(If c t e)
     (let ([u (uniquify-exp env)])
       (If (u c) (u t) (u e)))]
    [(Let x e body)
     (let* ([x1 (gensym x)]
            [e1 ((uniquify-exp env) e)]
            [env1 (dict-set env x x1)]
            [body1 ((uniquify-exp env1) body)])
       (Let x1 e1 body1))]
    [(Prim op es)
     (Prim op (for/list ([e es]) ((uniquify-exp env) e)))]))

(define (param-type p)
  (match p
    [`(,_ : ,t) t]
    [else (error "param-type passed non-param : " p)]))

(define (uniquify p)
  (match p
    [(ProgramDefs info def*)
     (let ([fun* (foldr (lambda (def a)
                          (cons (cons def def) a))
                        '()
                        def*)])
       (ProgramDefs info
                    (map (lambda (d)
                           (let ([param* (foldr (lambda (param a)
                                                  (cons (cons (param-name param)
                                                              (param-name param)) a))
                                                '()
                                                (Def-param* d))])
                             (struct-copy Def d
                                          [body ((uniquify-exp (append fun*
                                                                       param*)) (Def-body d))])))
                         def*)))]))
