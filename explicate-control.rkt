#lang racket

(require "utilities.rkt")

(provide explicate-control)

; c1 = Assign_1, Assign_2, ..., Assign_k, Ret1
; c2 = Assign_(k + 1), ..., Ret2
; Assign_1, Assign_2, ..., Assign_k, x = Ret1, Assign_(k + 1), ..., Ret2
(define (merge-conts c1 c2 x)
  (match c1
    [(Return v)   (Seq (Assign (Var x) v) c2)]
    [(Seq a tail) (Seq a (merge-conts tail c2 x))]
    [else         (error "Couldn't merge " c1 " and " c2)]))

; annotate return with type t
(define (annotate-cont c t)
  (match c
    [(Return v)   (Return (HasType v t))]
    [(Seq a tail) (Seq a (annotate-cont tail t))]
    [else         (error "Couldn't annotate ~a with ~a" c t)]))

(define (block->goto b label->block)
  (delay
    (let ([b (force b)])
      (match b
        [(Goto l) (Goto l)]
        [else (let ([l (gensym 'block)])
                (dict-set! label->block l b)
                (Goto l))]))))

(define (base? e)
  (match e
    [(or (Void) (Bool _) (Int _) (Var _) (Allocate _ _) (GlobalValue _)
           (Prim _ _) (Collect _) (FunRef _))
       #t]
    [else #f]))

(define (make-ifstmt c t e label->block)
  (IfStmt c
          (force (block->goto t label->block))
          (force (block->goto e label->block))))

(define (explicate-pred c t e label->block)
  (match c
    [(Bool b) (if b t e)]
    [(Var x)
     (delay (make-ifstmt (Prim 'eq? `(,(Var x) ,(Bool #t))) t e label->block))]
    [(Prim op es) #:when (member op '(eq? < vector-ref))
     (delay (make-ifstmt (Prim op es) t e label->block))]
    [(Apply fun arg*) (delay (Call fun arg*))]
    [(Prim 'not `(,c))
     (explicate-pred c e t label->block)]
    [(Let x rhs body)
     (let ([cont (explicate-pred body t e label->block)])
       (explicate-assign rhs x cont label->block))]
    [(If c1 t1 e1)
    ; Invariant : t/e will be used _at least_ once, so it is safe to lazily add
    ; them to the table now
     (let* ([t (block->goto t label->block)]
            [e (block->goto e label->block)]
            [t2 (explicate-pred t1 t e label->block)]
            [e2 (explicate-pred e1 t e label->block)])
       (explicate-pred c1 t2 e2 label->block))]
    [(HasType e type)
     (HasType (explicate-pred c t e label->block) type)]
    [else (error "explicate-pred unhandled case " c)]))

(define (explicate-assign rhs x cont label->block [annotate #f])
  ; Invaraint : only way to reach cont is after the assignment x = rhs
  (match rhs
    [_ #:when (base? rhs)
       (delay (Seq (Assign (Var x)
                           (if annotate
                             (HasType rhs annotate)
                             rhs))
                   (force cont)))]
    [(Apply fun arg*)
     (delay (Seq (Assign (Var x)
                         (Call fun arg*))
                 (force cont)))]
    [(Let y rhs body)
     (let ([body (explicate-assign body x cont label->block)])
       (explicate-assign rhs y body label->block))]
    [(If c t e)
     (let ([cont-t (explicate-assign t x cont label->block)]
           [cont-e (explicate-assign e x cont label->block)])
       (explicate-pred c cont-t cont-e label->block))]
    [(HasType e t)
     (explicate-assign e x cont label->block t)]
    [else (error "explicate-assign unhandled case" rhs)]))

(define (explicate-tail e label->block)
  (match e
    [_ #:when(base? e)
     (delay (Return e))]
    [(Apply fun arg*)
     (TailCall fun arg*)]
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-tail body label->block) label->block)]
    [(If c t e)
     (explicate-pred c
                     (explicate-tail t label->block)
                     (explicate-tail e label->block)
                     label->block)]
    [(HasType e t) (HasType (explicate-tail e label->block) t)]
    [else (error "explicate-tail unhandled case " e)]))

(define (ec-def d)
  (match d
    [(Def name param* rty info e)
     (let* ([label->block (make-hash)]
            [_ (dict-set! label->block
                          (string->symbol (string-append (symbol->string name)
                                                         "start"))
                          (force (explicate-tail e label->block)))])
       (Def name param* rty info (hash->list label->block)))]))

; Stuff that lazy evaluation achieves
; 1. Avoids duplicate block generation
; 2. Avoids unreachable block generation aka constant folding over booleans
(define (explicate-control p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info (map ec-def def*))]))
