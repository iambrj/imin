#lang racket
(require racket/set
         racket/stream
         racket/fixnum
         "interp-Rint.rkt"
         "utilities.rkt"
         graph
         "priority_queue.rkt")
(provide (all-defined-out))

(define caller-saved
  (let ([r '(rax rcx rdx rsi rdi r8 r9 r10 r11)])
    (map Reg r)))
(define callee-saved
  (let ([r '(rsp rbp rbx r12 r13 r14 r15)])
    (map Reg r)))
(define arg-regs
  (let ([r '(rdi rsi rdx rcx r8 r9)])
    (map Reg r)))
; '(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
(define usable-registers
  (let ([r '(rbx rcx rdx rsi rdi r8 r9 r10 r12 r13 r14)])
    (map Reg r)))

;; Partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n)  (Int (fx- 0 n))]
    [else     (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2))  (Int (fx+ n1 n2))]
    [(_ _)                (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n)              (Int n)]
    [(Prim 'read '())     (Prim 'read '())]
    [(Prim '- `(,e1))     (pe-neg (pe-exp e1))]
    [(Prim '+ `(,e1 ,e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Rint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

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
    ; (and e1 e1) == (if e1 e2 #f)
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
       (shrink-exp e))]))

(define (shrink p)
  (match p
    [(Program info e)
     (Program info (shrink-exp e))]))

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
    [(Void)   (Void)]
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Var v)  (Var v)]
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
    [(Program info e)
     (Program info (ea-exp e))]))

(define ((uniquify-exp env) e)
  (match e
    [(Var x)  (Var (dict-ref env x))]
    [(Int n)  (Int n)]
    [(Bool b) (Bool b)]
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

(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (atom? x)
  (or (Int? x)
      (Bool? x)
      (Var? x)
      (Void? x)))

(define (build-lets var->expr* body)
  (match var->expr*
    ['() body]
    [`((,var . ,expr) . ,d)
      (Let var expr (build-lets d body))]))

; returns atomic expression, new env
(define (rco-atm e)
  (match e
    [(Void)   (values (Void) '())]
    [(Int i)  (values (Int i) '())]
    [(Var v)  (values (Var v) '())]
    [(Bool b) (values (Bool b) '())]
    [(HasType e t) #:when (atom? e)
     (values (HasType e t) e)]
    [(HasType e t)
     (let-values ([(atm env) (rco-atm e)])
       ; Invariant : atm is *always* a (gensym 'tmp), since other cases are
       ; caught by previous clauses
       (values (HasType (Var atm) t)
               (dict-set env atm (HasType (dict-ref env atm) t))))]
    [(Collect bytes)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(Collect bytes)))))]
    [(GlobalValue var)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(GlobalValue var)))))]
    [(Allocate bytes t)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(Allocate bytes t)))))]
    [(Prim op es)
     (let* ([tmp (gensym 'tmp)]
            [atm-envs (map (compose (lambda (atm env)
                                       (cons atm env))
                                     rco-atm)
                            es)]
            ; Invariant : alist ordered envs preserve order of execution
            [envs (filter-map (lambda (x)
                                (and (not (empty? (cdr x)))
                                     (cdr x))) atm-envs)]
            [atms (map (compose (lambda (atm)
                                  (if (symbol? atm)
                                    (Var atm)
                                    atm))
                                car)
                       atm-envs)])
       (values tmp (append (apply append envs)
                           `((,tmp . ,(Prim op atms))))))]
    [(If c t e)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(If (rco-exp c) (rco-exp t) (rco-exp e))))))]
    [(Let x v b)
     (let ([v (rco-exp v)]
           [b (rco-exp b)])
       (if (atom? b)
         (values b `((,x . ,v)))
         (let ([tmp (gensym 'tmp)])
           (values tmp `((,tmp . ,(Let x (rco-exp v) (rco-exp b))))))))]))

(define (rco-exp e)
  (match e
    [(Void)             (Void)]
    [(Int i)            (Int i)]
    [(Var v)            (Var v)]
    [(Bool b)           (Bool b)]
    [(HasType e t)      (HasType (rco-exp e) t)]
    [(Allocate bytes t) (Allocate bytes t)]
    [(Collect bytes)    (Collect bytes)]
    [(GlobalValue var)  (GlobalValue var)]
    [(Prim op es)
     (let* ([atm-envs (map (compose (lambda (a e)
                                      (cons a e))
                                    rco-atm)
                           es)]
            [atms (map (compose (lambda (atm)
                                  (if (symbol? atm)
                                    (Var atm)
                                    atm))
                                car)
                       atm-envs)]
            ; Invariant : alist ordered envs preserve order of execution
            [envs (filter-map (lambda (x)
                                (and (not (empty? (cdr x)))
                                     (cdr x)))
                              atm-envs)])
       (build-lets (apply append envs) (Prim op atms)))]
    [(If c t e)
     (If (rco-exp c) (rco-exp t) (rco-exp e))]
    [(Let x v b) (Let x (rco-exp v) (rco-exp b))]))

; arguments of operations are atomic
(define (remove-complex-opera* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))

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
    [else         (error "Couldn't annotate ~s with ~s" c t)]))

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
           (Prim _ _) (Collect _))
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
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-tail body label->block) label->block)]
    [(If c t e)
     (explicate-pred c
                     (explicate-tail t label->block)
                     (explicate-tail e label->block)
                     label->block)]
    [(HasType e t) (HasType (explicate-tail e label->block) t)]
    [else (error "explicate-tail unhandled case " e)]))

; Stuff that lazy evaluation achieves
; 1. Avoids duplicate block generation
; 2. Avoids unreachable block generation aka constant folding over booleans
(define (explicate-control p)
  (match p
    [(Program info body)
     (let* ([label->block (make-hash)]
            [tail (explicate-tail body label->block)]
            [_ (dict-set! label->block 'start (force tail))])
       (CProgram info (hash->list label->block)))]))

(define (pmask t [mask 0])
  (match t
    [`(Vector) mask]
    [`(Vector (Vector . ,_))
      (bitwise-ior mask 1)]
    [`(Vector ,_) mask]
    [`(Vector . ((Vector . ,_) . ,rest))
      (pmask `(Vector . ,rest) (arithmetic-shift (bitwise-ior mask 1) 1))]
    [`(Vector . (,t . ,rest))
      (pmask `(Vector . ,rest) (arithmetic-shift mask 1))]
    [else (error "Couldn't make pmask for " t)]))

; second argument of cmp cannot be an immediate, generate temorary mov if it is
(define (cmp-tmp-mov a)
  (match (si-atm a)
    [(Imm a)
     (values `(,(Instr 'movq `(,(Imm a) ,(Reg 'rax)))) (Reg 'rax))]
    [_ (values '() a)]))

(define (si-atm e)
  (match e
    [(Int n) (Imm n)]
    [(Var v) (Var v)]
    [(Bool #f) (Imm 0)]
    [(Bool #t) (Imm 1)]
    [(GlobalValue g) (Global g)]
    [(HasType x _)  (si-atm x)]
    [else (error "si-atm unhandled case " e)]))

(define (si-stmt e)
  (match e
    [(Assign (Var v) (HasType e t)) (si-stmt (Assign (Var v) e))]
    [(Assign (Var v) (Int i)) `(,(Instr 'movq `(,(Imm i) ,(Var v))))]
    [(Assign (Var v) (Bool b)) `(,(Instr 'movq `(,(si-atm (Bool b)) ,(Var v))))]
    [(Assign (Var v) (Var u)) `(,(Instr 'movq `(,(Var u) ,(Var v))))]
    [(Assign (Var v) (GlobalValue g)) `(,(Instr 'movq `(,(Global g) ,(Var v))))]
    [(Assign (Var v) (Void)) `(,(Instr 'movq `(,(Imm 0) ,(Var v))))]
    [(Assign (Var v) (Prim 'read '()))
     `(,(Callq `read_int 0)
        ,(Instr 'movq `(,(Reg 'rax) ,(Var v))))]
    [(Assign (Var v) (Prim '- `(,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,a ,(Var v)))
          ,(Instr 'negq `(,(Var v)))))]
    [(Assign (Var v) (Prim '+ `(,a1 ,a2)))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       (cond
         ; v = (+ v a2)
         [(equal? (Var v) a1) `(,(Instr 'addq `(,(si-atm a2) ,(Var v))))]
         ; v = (+ a1 v)
         [(equal? (Var v) a2) `(,(Instr 'addq `(,(si-atm a1) ,(Var v))))]
         ; v = (+ a1 a2)
         [else `(,(Instr 'movq `(,a1 ,(Var v))) ,(Instr 'addq `(,a2 ,(Var v))))]))]
    [(Assign (Var v) (Prim 'not `(,(Var v))))
     `(,(Instr 'xorq `(,(Imm 1) ,(Var v))))]
    [(Assign (Var v) (Prim 'not `(,a)))
     (let ([a (si-atm a)])
     `(,(Instr 'movq `(,a ,(Var v)))
       ,(Instr 'xorq `(,(Imm 1) ,(Var v)))))]
    [(Assign (Var v) (Prim 'eq? `(,a1 ,a2)))
     (let-values ([(tmp a1) (cmp-tmp-mov a1)]
                  [(a2) (si-atm a2)])
       (append tmp
               `(,(Instr 'cmpq `(,a2 ,a1))
                  ,(Instr 'set `(e ,(Reg 'al)))
                  ,(Instr 'movzbq `(,(Reg 'al) ,(Var v))))))]
    [(Assign (Var v) (Prim '< `(,a1 ,a2)))
     (let-values ([(tmp a1)  (cmp-tmp-mov a1)]
                  [(a2) (si-atm a2)])
       (append tmp
               `(,(Instr 'cmpq `(,a2 ,a1))
                  ,(Instr 'set `(l ,(Reg 'al)))
                  ,(Instr 'movzbq `(,(Reg 'al) ,(Var v))))))]
    [(Assign (Var x) (Prim 'vector-ref `(,v ,(Int n))))
     `(,(Instr 'movq `(,v ,(Reg 'r11)))
        ,(Instr 'movq `(,(Deref 'r11 (* 8 (+ n 1))) ,(Var x))))]
    [(Assign (Var x) (Prim 'vector-set! `(,v ,(Int n) ,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,v ,(Reg 'r11)))
          ,(Instr 'movq `(,a ,(Deref 'r11 (* 8 (+ n 1)))))
          ,(Instr 'movq `(,(Imm 0) ,(Var x)))))]
    ; Invariant : allocate is only called when it is guaranteed that there is
    ; enough space
    ; Invariant : allocate can only appear in rhs of a let
    [(Assign (Var v) (Allocate len t))
     (let ([tag (bitwise-ior 1 ; In FromSpace, not yet copied
                             (arithmetic-shift len 1) ; size of tuple
                             (arithmetic-shift (pmask t) 7))])
       `(,(Instr 'movq `(,(Global 'free_ptr) ,(Reg 'r11)))
          ,(Instr 'addq `(,(Imm (* 8 (+ len 1))) ,(Global 'free_ptr)))
          ,(Instr 'movq `(,(Imm tag) ,(Deref 'r11 0)))
          ,(Instr 'movq `(,(Reg 'r11) ,(Var v)))))]
    [(Assign (Var v) (Collect bytes))
     `(,(Instr 'movq `(,(Reg 'r15) ,(Reg 'rdi)))
        ,(Instr 'movq `(,(Imm bytes) ,(Reg 'rsi)))
        ,(Callq 'collect 2))]
    [else (error "si-stmt unhandled case : " e)]))

(define (si-tail e)
  (match e
    [(Return (Var v))  `(,(Instr 'movq `(,(Var v) ,(Reg 'rax))) ,(Jmp 'conclusion))]
    [(Return (Int i))  `(,(Instr 'movq `(,(Imm i) ,(Reg 'rax))) ,(Jmp 'conclusion))]
    [(Return (Prim 'read '()))
     `(,(Callq 'read_int 0)
        ,(Jmp 'conclusion))]
    [(Return (Prim '- `(,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,a ,(Reg 'rax)))
         ,(Instr 'negq `(,(Reg 'rax)))
         ,(Jmp 'conclusion)))]
    [(Return (Prim '+ `(,a1 ,a2)))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'movq `(,a1 ,(Reg 'rax)))
         ,(Instr 'addq `(,a2 ,(Reg 'rax)))
         ,(Jmp 'conclusion)))]
    [(Return (Prim 'vector-ref `(,v ,(Int n))))
     `(,(Instr 'movq `(,v ,(Reg 'r11)))
        ,(Instr 'movq `(,(Deref 'r11 (* 8 (+ n 1))) ,(Reg 'rax)))
        ,(Jmp 'conclusion))]
    [(Return (Prim 'vector-set! `(,v ,(Int n) ,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,v ,(Reg 'r11)))
          ,(Instr 'movq `(,a ,(Deref 'r11 (* 8 (+ n 1)))))
          ,(Instr 'movq `(,(Imm 0) ,(Reg 'rax)))
          ,(Jmp 'conclusion)))]
    [(Seq stmt tail)
     (let ([s (si-stmt stmt)]
           [t (si-tail tail)])
       (append s t))]
    [(Goto l) `(,(Jmp l))]
    [(IfStmt (Prim 'eq? `(,a1 ,a2)) (Goto l1) (Goto l2))
     (let-values ([(tmp a1) (cmp-tmp-mov a1)]
                  [(a2) (si-atm a2)])
       (append tmp
               `(,(Instr 'cmpq `(,a2 ,a1))
                  ,(JmpIf 'e l1)
                  ,(Jmp l2))))]
    [(IfStmt (Prim '< `(,a1 ,a2)) (Goto l1) (Goto l2))
     (let-values ([(tmp a1) (cmp-tmp-mov a1)]
                  [(a2) (si-atm a2)])
       (append tmp
               `(,(Instr 'cmpq `(,a2 ,a1))
                  ,(JmpIf 'l l1)
                  ,(Jmp l2))))]
    ; Invariant : grammar restricts to immediate integer references, don't have
    ; to worry about expressions evaluating to integers
    [(IfStmt (Prim 'vector-ref `(,v ,(Int i))) (Goto l1) (Goto l2))
       `(,(Instr 'movq `(,v ,(Reg 'r11)))
          ,(Instr 'cmpq `(,(Imm 1) ,(Deref 'r11 (* 8 (+ i 1)))))
          ,(JmpIf 'e l1)
          ,(Jmp l2))]
    [else (error "si-tail unhandled case : " e)]))

; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info label-tails)
     (let* ([label-blocks (map (lambda (label-tail)
                                 `(,(car label-tail) . ,(Block '() (si-tail (cdr label-tail))))) label-tails)])
       (X86Program info label-blocks))]))

(define ((add-edges-instr! g c) instr)
  (match instr
    [(Jmp l)
     (add-directed-edge! g c l)]
    [(JmpIf _ l)
     (add-directed-edge! g c l)]
    [else (void)]))

(define ((add-edges! g) label-block)
  (match (cdr label-block)
    [(Block info instrs)
     (map (add-edges-instr! g (car label-block)) instrs)]
    [else (error "add-edges! unhandled case " label-block)]))

(define (build-cfg p)
  (match p
    [(X86Program info label-blocks)
     (let* ([labels (dict-keys label-blocks)]
            [g (directed-graph '())]
            [_ (foldr (lambda (v acc)
                        (add-vertex! g v))
                      g
                      labels)]
            [_ (map (add-edges! g) label-blocks)])
       (printf "CFG : ~s\n" (graphviz g))
       (X86Program (dict-set info 'cfg g) label-blocks))]))

; XXX : hardocded to al, will have to change if other registers are used
(define (byte->reg a)
  (match a
    [(Reg 'al) (Reg 'rax)]
    [_ a]))

(define (instr-w instr)
  (match instr
    [(Instr i `(,_ ,a2)) #:when (member i '(movq addq subq xorq movzbq set))
     (ul-arg (byte->reg a2))]
    [(Instr 'negq `(,arg)) (ul-arg arg)]
    [(Instr 'cmpq _) (set)]
    [(Jmp _) (set)]
    [(JmpIf _ _) (set)]
    [(Callq _ _) (list->set caller-saved)]
    [else (error "instr-w missing case : " instr)]))

(define (instr-r instr)
  (match instr
    [(Instr i `(,a1 ,a2)) #:when (member i '(addq subq xorq cmpq))
     (set-union (ul-arg a1) (ul-arg a2))]
    [(Instr i `(,a1 ,_)) #:when (member i '(movq subq movzbq))
     (ul-arg a1)]
    [(Instr 'negq `(,arg)) (ul-arg arg)]
    [(Instr 'set _) (set)]
    [(Callq _ i) (list->set (take callee-saved i))]
    [else (error "instr-r missing case : " instr)]))

(define (ul-arg a)
  (match a
    [(or (Var _) (Reg _)) (set (byte->reg a))]
    [(or (Bool _) (Int _) (Imm _)) (set)]))

; Lafter(n) = {}
; Lafter(k) =  Lbefore(k - 1)
; Lbefore(k) = (Lafter(k) - W(k)) U R(k)
; Thus, Lafter(k) = Lafter(k + 1) - W(k) U R(k)
(define (ul-instrs instrs after label->liveafter)
  (match instrs
    ['() after]
    [(or `(,(Jmp label) . ,t) `(,(JmpIf _ label) . ,t))
     (let ([a (if (dict-has-key? label->liveafter label)
                (dict-ref label->liveafter label)
                (error "liveafter not built for Jmp " label))])
       (ul-instrs t (cons (set-union a (car after)) after) label->liveafter))]
    [`(,i . ,t)
      (let ([w (instr-w i)]
            [r (instr-r i)]
            [a (car after)])
        (ul-instrs t (cons (set-union (set-subtract a w) r) after) label->liveafter))]))

(define (ul-block l blk label->liveafter)
  (match blk
    [(Block info instrs)
     (let* ([liveafter (ul-instrs (reverse instrs) `(,(set)) label->liveafter)]
            [_ (dict-set! label->liveafter l (car liveafter))])
       (cons l (Block (dict-set info 'live-after liveafter) instrs)))]
    [else (error "ul-block unhandled case " blk)]))

(define (uncover-live p)
  (match p
    [(X86Program info label-blocks)
     (let* ([cfg (dict-ref info 'cfg)]
            ; conclusion is always last, remove it since not yet generated
            [order (cdr (tsort (transpose cfg)))]
            [label->liveafter (make-hash `((conclusion . ,(set (Reg 'rax) (Reg 'rsp)))))]
            [label-blocks (map (lambda (label)
                                 (ul-block label
                                           (dict-ref label-blocks label)
                                           label->liveafter))
                               order)])
       (X86Program info label-blocks))]))

(define (bi-instr instr live-after g)
  (match instr
    [(Instr 'movq `(,s ,d))
     (let ([_ (for ([v (set->list live-after)])
                (unless (or (equal? v d) (equal? v s)) (add-edge! g d v)))])
       g)]
    [else (let* ([w (instr-w instr)]
                 [_ (for* ([d w]
                           [v live-after])
                      (unless (equal? v d) (add-edge! g d v)))])
            g)]))

(define ((bi-block g) blk)
  (match blk
    [(Block info instrs)
     (let* ([live-after (dict-ref info 'live-after)]
            [vertices (apply set-union live-after)]
            [_ (for ([v (set->list vertices)]) (add-vertex! g v))]
            [_ (for ([i instrs] [la live-after]) (bi-instr i la g))])
       ; Return (Block (dict-set info 'conflicts g) instrs) instead if want
       ; block-wise interference graphs
       g)]))

(define (build-interference p)
  ; XXX : hardcoding for single start block, will have to fix when language has
  ; more features
  (match p
    [(X86Program info label-blocks)
     (let* ([g (undirected-graph '())]
            [_ (map (compose (bi-block g) cdr) label-blocks)])
       (X86Program (dict-set info 'conflicts g) label-blocks))]))

(define (min-free-list l)
  (match l
    [`(,a) (add1 a)]
    [`(,x . (,y . ,d))
      (if (= (add1 x) y)
        (min-free-list `(,y . ,d))
        (add1 x))]))

; NOTE : Can be optimized to O(n) using some obscure mutation tricks
(define (min-free s)
  (let* ([l (set->list s)]
         [l (sort l <)])
    (if (or (equal? '() l) (> (car l) 0))
      0
      (min-free-list l))))

(define (update-neighbors! vertex->satur color neighbors)
  (match neighbors
    [`() vertex->satur]
    [`(,n . ,d)
      (let* ([s (dict-ref vertex->satur n)]
             [_ (dict-set! vertex->satur n (set-union s (set color)))])
        (update-neighbors! vertex->satur color d))]))

(define (color-graph g q w vertex->satur vertex->node vertex->color)
  (match w
    ['() vertex->color]
    [else (let* ([v (pqueue-pop! q)]
                 [w (set-remove w v)]
                 [satur (dict-ref vertex->satur v)]
                 [color (min-free satur)]
                 [neighbors (filter Var? (get-neighbors g v))]
                 [_ (foldr (lambda (x a)
                             (pqueue-decrease-key! q (dict-ref vertex->node x)))
                           '()
                           neighbors)]
                 [vertex->satur (update-neighbors! vertex->satur color neighbors)])
            (color-graph g q w vertex->satur vertex->node (dict-set
                                                            vertex->color v
                                                            color)))]))

(define (var->mem var->color color->reg)
  (let* ([maxcolor (apply max (dict-keys color->reg))]
         [spilled->stk (foldr (lambda (k a)
                                (if (> (dict-ref var->color k)
                                       maxcolor)
                                  (dict-set a k (- (dict-ref var->color k) maxcolor))
                                  a))
                              '()
                              (dict-keys var->color))]
         [color->stk (foldr (lambda (color a)
                              (dict-set a color (Deref 'rbp (* -8 color))))
                            '()
                            (dict-values spilled->stk))]
         [color->reg (append color->reg color->stk)])
    (values (dict-map var->color (lambda (v c)
                                   `(,v . ,(dict-ref color->reg c))))
            (length (dict-values spilled->stk)))))

(define ((ar-arg var->mem) arg)
  (match arg
    [(Var v) (dict-ref var->mem (Var v))]
    [_ arg]))

(define ((ar-instr var->mem) instr)
  (match instr
    [(Instr i args) (Instr i (map (ar-arg var->mem) args))]
    [_ instr]))

(define ((ar-block var->mem) blk)
  (match blk
    [(Block info instrs)
     (Block info (map (ar-instr var->mem) instrs))]))

(define (allocate-registers p)
  (match p
    [(X86Program info label-blocks)
     (let*-values ([(g) (dict-ref info 'conflicts)]
                   [(w) (filter Var? (get-vertices g))]
                   [(r) (filter Reg? (get-vertices g))]
                   [(vertex->color) (foldr (lambda (r a)
                                             (if (member r usable-registers)
                                               (dict-set a r (index-of
                                                               usable-registers
                                                               r))
                                               a))
                                           '()
                                           r)]
                   [(vertex->sat)
                    (make-hash (foldr (lambda (v a)
                                        (let* ([n (get-neighbors g v)]
                                               [s (foldr (lambda (n a)
                                                           (if (dict-has-key? vertex->color n)
                                                             (set-union a (set (dict-ref vertex->color n)))
                                                             a))
                                                         (set)
                                                         n)])
                                          `((,v . ,s) . ,a))) '() w))]
                   [(q)
                    (make-pqueue (lambda (x1 x2)
                                   (> (set-count (dict-ref vertex->sat x1))
                                      (set-count (dict-ref vertex->sat x2)))))]
                   [(vertex->node)
                    (map (lambda (v) (cons v (pqueue-push! q v))) w)]
                   [(vertex->color)
                    (color-graph g q w vertex->sat vertex->node vertex->color)]
                   [(var->mem stack-space)
                    (var->mem vertex->color (map (lambda (r)
                                                   (cons (index-of usable-registers r) r))
                                                 usable-registers))]
                   [(used-callee) (filter (lambda (r)
                                            (member r callee-saved))
                                          (remove-duplicates (dict-values var->mem)))])
       (X86Program (dict-set* info
                              'stack-space stack-space
                              'var->mem var->mem
                              'used-callee used-callee)
                   (map (lambda (label-block)
                          (cons (car label-block)
                                ((ar-block var->mem) (cdr label-block))))
                        label-blocks)))]))

(define (trivial-mov instr)
  (match instr
    [(Instr 'movq `(,a ,a)) #t]
    [_ #f]))

(define (pi-instr instr)
  (match instr
    [(Instr op `(,(Deref 'rbp x) ,(Deref 'rbp y)))
     ; What if rax is live at this point!?
     `(,(Instr 'movq `(,(Deref 'rbp x) ,(Reg 'rax)))
        ,(Instr op `(,(Reg 'rax) ,(Deref 'rbp y))))]
    [_ `(,instr)]))

(define (pi-block blk)
  (match blk
    [(Block info instrs)
     (let* ([instrs (filter (compose not trivial-mov) instrs)]
            [instrs (foldl (lambda (i a)
                             (append a (pi-instr i)))
                           '()
                           instrs)])
       ; really necessary?
       (Block info (filter (compose not trivial-mov) instrs)))]))

;; patch-instructions : psuedo-x86 -> x86
; 1. Remove trivial moves e.g. mov rax rax
; 2. Remove multi memory accesses in same instruction e.g. transform mov -8rbp,
; -16rbp to mov -8rbp, rax + mov rax, -16rbp
(define (patch-instructions p)
  (match p
    [(X86Program info label-blocks)
     (let ([label-blocks (map (lambda (label-block)
                                (cons (car label-block)
                                      (pi-block (cdr label-block))))
                              label-blocks)])
       (X86Program info label-blocks))]))

(define (print-x86-arg arg)
  (match arg
    [(Imm i) (string-append "$" (number->string i))]
    [(Reg r) (string-append "%" (symbol->string r))]
    [(Deref r i) (string-append (number->string i) "(%" (symbol->string r) ")")]
    [else (error "print-x86-arg unhandled case " arg)]))

(define (print-x86-args args)
  (match args
    [`(,a) (print-x86-arg a)]
    [`(,a1 ,a2)
      (string-append (print-x86-arg a1) ", " (print-x86-arg a2))]))

(define (print-x86-instr instr)
  (match instr
    [(Callq l i) (string-append "callq "
                                (symbol->string l))]
    [(Retq) "retq"]
    [(Jmp l) (string-append "jmp " (symbol->string l))]
    [(JmpIf cc l) (string-append "j"
                                 (symbol->string cc)
                                 " "
                                 (symbol->string l))]
    [(Instr 'set `(,cc ,r))
     (string-append "set"
                    (symbol->string cc)
                    " "
                    (print-x86-arg r))]
    [(Instr op args)
     (string-append (symbol->string op) " "
                    (print-x86-args args))]))

(define (print-x86-block blk)
  (match blk
    [(Block info instrs)
     (foldr (lambda (instr acc)
              (string-append "\t" (print-x86-instr instr) "\n" acc))
            ""
            instrs)]
    [_ (error "print-x86-block unhandled case " blk)]))

(define (print-main-block stack-space used-callee)
  (string-append "main:\n"
                 "\tpushq %rbp\n"
                 (apply string-append
                        (map (lambda (r)
                               (string-append "\tpushq %"
                                              (symbol->string (Reg-name r))
                                              "\n"))
                             used-callee))
                 "\tmovq %rsp, %rbp\n"
                 "\tsubq $" (number->string (if (zero? (modulo (+ stack-space
                                                                  (* 8 (length used-callee)))
                                                               16))
                                              stack-space
                                              (+ 8 stack-space))) ", %rsp\n"
                 "\tjmp start\n"))

(define (print-conclusion-block stack-space used-callee)
  (string-append "conclusion:\n"
                 "\taddq $" (number->string (if (zero? (modulo (+ stack-space
                                                                  (* 8 (length used-callee)))
                                                               16))
                                              stack-space
                                              (+ 8 stack-space))) ", %rsp\n"
                 (apply string-append
                        (map (lambda (r)
                               (string-append "\tpopq %"
                                              (symbol->string (Reg-name r))
                                              "\n"))
                             used-callee))
                 "\tpopq %rbp\n"
                 "\tretq\n"))

;; print-x86 : x86 -> string
(define (print-x86 p)
  (match p
    [(X86Program info label-blocks)
     (let* ([stack-space (dict-ref info 'stack-space)]
            [used-callee (dict-ref info 'used-callee)]
            [main (print-main-block stack-space used-callee)]
            [conclusion (print-conclusion-block stack-space used-callee)]
            [label-blocks (foldl (lambda (label-block acc)
                                   (string-append (symbol->string (car label-block)) ":\n"
                                                  (print-x86-block (cdr label-block))
                                                  (if (equal? (car label-block) 'start)
                                                    "\t.globl main\n"
                                                    "")
                                                  acc))
                                 ""
                                 label-blocks)])
       (string-append label-blocks main conclusion))]))
