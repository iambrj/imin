#lang racket

(require "utilities.rkt"
         "utils.rkt")

(provide expose-allocation)

(define (ccollect bytes)
  (If (Prim '< `(,(Prim '+ `(,(HasType (GlobalValue 'free_ptr) 'Integer) ,(Int bytes)))
                  ,(HasType (GlobalValue 'fromspace_end) 'Integer)))
      (Void)
      (Collect bytes)))

; 1. Evaluate the components of the tuple into let bindings
; 2. Place conditional call to collect
; 3. Call allocate
; 4. Initalize vector
(define (ea-exp e)
  (match e
    [(Void)           (Void)]
    [(Int i)          (Int i)]
    [(Bool b)         (Bool b)]
    [(Var v)          (Var v)]
    [(FunRef f)       (FunRef f)]
    [(Apply fun arg*) (Apply (ea-exp fun) (map ea-exp arg*))]
    [(HasType (Prim 'vector es) t)
     (let* ([vname (gensym 'v)]
            [len (length es)]
            [bytes (+ 8 (* len 8))]
            [xs (map (lambda (x) (gensym 'x)) es)]
            [es (map ea-exp es)]
            [ccol (ccollect bytes)]
            [aloc (Allocate len t)]
            [sets (map (lambda (x n)
                         (parse-exp `(vector-set! ,vname
                                                  ,n
                                                  ,(list-ref xs n))))
                       xs (range 0 (length xs)))]
            [var->expr (foldr (lambda (v r a)
                                `((,v . ,r) . ,a))
                              '()
                              (append xs
                                      `(_ ,vname)
                                      (make-list len '_))
                              (append es
                                      `(,ccol ,aloc)
                                      sets))])
       (build-lets var->expr (parse-exp vname)))]
    [(Let v e b) (Let v (ea-exp e) (ea-exp b))]
    [(If c t e) (If (ea-exp c) (ea-exp t) (ea-exp e))]
    [(Prim op es) #:when (member op '(eq? read - + not < vector-length))
     (Prim op (map ea-exp es))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (Prim 'vector-ref `(,(ea-exp e) ,(Int i)))]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (Prim 'vector-set! `(,(ea-exp e1) ,(Int i) ,(ea-exp e2)))]))

(define (expose-allocation p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info
                  (map (lambda (d)
                         (struct-copy Def d
                                      [body (ea-exp (Def-body d))]))
                       def*))]))
