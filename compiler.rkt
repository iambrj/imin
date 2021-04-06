#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Rint.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Rint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- `(,e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ `(,e1 ,e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Rint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
       (let* ([x1 (gensym x)]
              [e1 ((uniquify-exp env) e)]
              [env1 (dict-set env x x1)]
              [body1 ((uniquify-exp env1) body)])
         (Let x1 e1 body1))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))


(define (rco-atm e)
  (match e
    [(Int i)
     (values (Int i)
             '())]
    [(Var v)
     (values (Var v)
             '())]
    [(Prim 'read '())
     (let ([tmp (gensym 'tmp)])
      (values tmp
             `((,tmp . ,(Prim 'read '())))))]
    [(Prim '- `(,e1))
     (let ([tmp (gensym 'tmp)])
       (values tmp
               `((,tmp . ,(Prim '- `(,e1))))))]
    [(Prim '+ `(,e1 ,e2))
     (define-values (tmp1 tmp->exp1) (rco-atm e1))
     (define-values (tmp2 tmp->exp2) (rco-atm e2))
     (cond
       [(and (dict-empty? tmp->exp1) (dict-empty? tmp->exp2))
        (Prim '+ `(,e1 ,e2))]
       [(and (not (dict-empty? tmp->exp1)) (dict-empty? tmp->exp2))
        (values (Prim '+ `(,(Var tmp1) ,e2))
                tmp->exp1)]
       [(and (dict-empty? tmp->exp1) (not (dict-empty? tmp->exp2)))
        (values (Prim '+ `(,e1 ,(Var tmp2)))
                tmp->exp2)]
       [(and (not (dict-empty? tmp->exp1)) (not (dict-empty? tmp->exp2)))
        (values (Prim '+ `(,(Var tmp1) ,(Var tmp2)))
                `((,tmp1 . ,(dict-ref tmp->exp1 tmp1))
                  (,tmp2 . ,(dict-ref tmp->exp2 tmp2))))])]
    [(Let x v b)
     (let ([tmp (gensym 'tmp)])
       (values tmp
               `((,tmp . ,(Let x (rco-exp v) (rco-exp b))))))]))

(define (rco-exp e)
  (match e
    [(Int i) (Int i)]
    [(Var v) (Var v)]
    [(Prim 'read '())
     (Prim 'read '())]
    [(Prim '- `(,e1))
     (define-values (tmp tmp->expr) (rco-atm e1))
     (if (dict-empty? tmp->expr)
       (Prim '- `(,e1))
       (Let tmp (dict-ref tmp->expr tmp) (Prim '- `(,(Var tmp)))))]
    [(Prim '+ `(,e1 ,e2))
     (define-values (tmp1 tmp->exp1) (rco-atm e1))
     (define-values (tmp2 tmp->exp2) (rco-atm e2))
     (cond
       [(and (dict-empty? tmp->exp1) (dict-empty? tmp->exp2))
        (Prim '+ `(,e1 ,e2))]
       [(and (not (dict-empty? tmp->exp1)) (dict-empty? tmp->exp2))
        (Let tmp1 (dict-ref tmp->exp1 tmp1) (Prim '+ `(,(Var tmp1) ,e2)))]
       [(and (dict-empty? tmp->exp1) (not (dict-empty? tmp->exp2)))
        (Let tmp2 (dict-ref tmp->exp2 tmp2) (Prim '+ `(,e1 ,(Var tmp2))))]
       [(and (not (dict-empty? tmp->exp1)) (not (dict-empty? tmp->exp2)))
        (Let tmp1
             (dict-ref tmp->exp1 tmp1)
             (Let tmp2
                  (dict-ref tmp->exp2 tmp2)
                  (Prim '+ `(,(Var tmp1) ,(Var tmp2)))))])]
    [(Let x v b)
     (Let x (rco-exp v) (rco-exp b))]))

;; remove-complex-opera* : R1 -> R1
;; arguments of operations are atomic
(define (remove-complex-opera* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (error "TODO: code goes here (explicate-control)"))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (error "TODO: code goes here (select-instructions)"))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; print-x86 : x86 -> string
(define (print-x86 p)
  (error "TODO: code goes here (print-x86)"))
