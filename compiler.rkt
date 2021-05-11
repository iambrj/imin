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

(define (atom? e)
  (match e
    [(Int i) #t]
    [(Var v) #t]
    [else #f]))

(define (rco-atm e)
  (match e
    [(Int i) (values (Int i) '())]
    [(Var v) (values (Var v) '())]
    [(Prim 'read '())
     (let ([tmp (gensym 'tmp)]) (values tmp `((,tmp . ,(Prim 'read '())))))]
    [(Prim '- `(,e1))
     (let ([tmp (gensym 'tmp)])
       (let-values ([(atm atm->subexpr) (rco-atm e1)])
         (values tmp (append atm->subexpr `((,tmp . ,(Prim '- `(,atm))))))))]
         ; XXX : racket doesn't have dict-union (yet) :/, forced to
         ; exploit dict representation
    [(Prim '+ `(,e1 ,e2))
     (let ([tmp (gensym 'tmp)])
       (let-values ([(atm1 atm->subexpr1) (rco-atm e1)]
                    [(atm2 atm->subexpr2) (rco-atm e2)])
         (values tmp (append atm->subexpr1
                             atm->subexpr2
                             `((,tmp . ,(Prim '+ `(,atm1 ,atm2))))))))]
    [(Let x v b)
     (let ([v (rco-exp v)]
           [b (rco-exp b)])
       (if (atom? b)
         (values b `((,x . ,v)))
         (let ([tmp (gensym 'tmp)])
           (values tmp `((,tmp . ,(Let x (rco-exp v) (rco-exp b))))))))]))

(define (build-lets var->expr* body)
  (match var->expr*
    ['() body]
    [`((,var . ,expr) . ,d)
      (Let var expr (make-lets d body))]))

(define (rco-exp e)
  (match e
    [(Int i) (Int i)]
    [(Var v) (Var v)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- `(,e1))
     (let-values ([(atm atm->expr) (rco-atm e1)])
       (let ([atm (if (symbol? atm) (Var atm) atm)])
         (make-lets atm->expr (Prim '- `(,atm)))))]
    [(Prim '+ `(,e1 ,e2))
     (let-values ([(atm1 atm->subexpr1) (rco-atm e1)]
                  [(atm2 atm->subexpr2) (rco-atm e2)])
       (let ([atm1 (if (symbol? atm1) (Var atm1) atm1)]
             [atm2 (if (symbol? atm2) (Var atm2) atm2)])
         (make-lets (append atm->subexpr1 atm->subexpr2) (Prim '+ `(,atm1 ,atm2)))))]
    [(Let x v b) (Let x (rco-exp v) (rco-exp b))]))

;; remove-complex-opera* : R1 -> R1
;; arguments of operations are atomic
(define (remove-complex-opera* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))

(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body)
     (let ([cont (explicate-tail body)])
       (explicate-assign rhs x cont))]
    [(Prim op es)
     (Return (Prim op es))]
    [else (error "explicate-tail was passed a non tail expression : " e)]))

(define (merge-conts c1 c2 x)
  (match c1
    [(Return v)
     (Seq (Assign (Var x) v) c2)]
    [(Seq a tail)
     (Seq a (merge-conts tail c2 x))]
    [else (error "Couldn't merge : " c1 c2)]))

(define (explicate-assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body)
     (let* ([cont1 (explicate-tail body)]
            [cont (merge-conts cont1 cont x)])
       (explicate-assign rhs y cont))]
    [(Prim op es)
     (Seq (Assign (Var x) e) cont)]
    [else (error "explicate-tail unhandled case" e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body)
     (let ([tail (explicate-tail body)]) (CProgram '() `((start . ,tail))))]))

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
