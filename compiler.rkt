#lang racket
(require racket/set
         racket/stream
         racket/fixnum
         "interp-Rint.rkt"
         "utilities.rkt"
         graph
         "priority_queue.rkt")
(provide (all-defined-out))

(define framesize 16)

(define (round-frame i)
  (* 16 (exact-ceiling (/ i 16.))))

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
  (let ([r '(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)])
    (map Reg r)))

;; Partial evaluation pass described in the book.
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

(define (shrink-exp e)
  (match e
    [(Int i) (Int i)]
    [(Bool b) (Bool b)]
    [(Var v) (Var v)]
    [(Let v e b) (Let v (shrink-exp e) (shrink-exp b))]
    [(If c t e) (If (shrink-exp c) (shrink-exp t) (shrink-exp e))]
    [(Prim '- `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (Prim '+ `(,e1 ,(Prim '- `(,e2)))))]
    [(Prim '+ es) (Prim '+ (map shrink-exp es))]
    [(Prim '- `(,e))
     (let ([e (shrink-exp e)])
       (Prim '- `(,e)))]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim 'not `(,e))
     (let ([e (shrink-exp e)])
       (Prim 'not `(,(shrink-exp e))))]
    [(Prim 'and `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (If e1 e2 (Bool #f)))]
    [(Prim 'or `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (If e1 (Bool #t) e2))]
    [(Prim '< es) (Prim '< (map shrink-exp es))]
    [(Prim 'eq? es) (Prim 'eq? (map shrink-exp es))]
    ; (<= e1 e2) == (or (< e1 e2) (eq? e1 e2))
    ; Let bindings are creating to deal with (read)s
    [(Prim '<= `(,e1 ,e2))
     (let* ([v1 (gensym 'var)]
            [v2 (gensym 'var)]
            [e (Let v1 e1
                    (Let v2 e2
                         (Prim 'or `(,(Prim '< `(,v1 ,v2))
                                     ,(Prim 'eq? `(,v1 ,v2))))))])
       (shrink-exp e))]
    ; (> e1 e2) == (not (or (< e1 e2) (eq? e1 e2)))
    ; Let bindings are creating to deal with (read)s
    [(Prim '> `(,e1 ,e2))
     (let* ([v1 (gensym 'var)]
            [v2 (gensym 'var)]
            [e (Let v1 e1
                    (Let v2 e2
                         (Prim 'not `(,(Prim 'or `(,(Prim '< `(,v1 ,v2))
                                                   ,(Prim 'eq? `(,v1 ,v2))))))))])
       (shrink-exp e))]
    [(Prim '>= `(,e1 ,e2))
     (let ([e1 (shrink-exp e1)]
           [e2 (shrink-exp e2)])
       (Prim 'not `(,(Prim `< `(,e1 ,e2)))))]))

(define (shrink p)
  (match p
    [(Program info e)
     (Program info (shrink-exp e))]))

; uniquify-exp -> remove-complex-opera* -> explicate-control ->
; select-instructions -> assign-homes -> patch-instructions -> print x86

(define ((uniquify-exp env) e)
  (match e
    [(Var x)
     (Var (dict-ref env x))]
    [(Int n) (Int n)]
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

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (atom? e)
  (match e
    [(Int i) #t]
    [(Var v) #t]
    [else #f]))

; returns atomic expression, new env
(define (rco-atm e)
  (match e
    [(Int i) (values (Int i) '())]
    [(Var v) (values (Var v) '())]
    [(Bool b) (values (Bool b) '())]
    ; XXX : cleaner way to handle all Prims in one go? map a procedure that
    ; returns multiple values?
    [(Prim 'read '())
     (let ([tmp (gensym 'tmp)]) (values tmp `((,tmp . ,(Prim 'read '())))))]
    [(Prim op `(,e1)) #:when (member op '(- not))
     (let ([tmp (gensym 'tmp)])
       (let-values ([(atm atm->subexpr) (rco-atm e1)])
         (values tmp (append atm->subexpr `((,tmp . ,(Prim op `(,atm))))))))]
    [(Prim op `(,e1 ,e2)) #:when (member op '(eq? < +))
     (let ([tmp (gensym 'tmp)])
       (let-values ([(atm1 atm->subexpr1) (rco-atm e1)]
                    [(atm2 atm->subexpr2) (rco-atm e2)])
         (values tmp (append atm->subexpr1
                             atm->subexpr2
                             `((,tmp . ,(Prim op `(,atm1 ,atm2))))))))]
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

(define (build-lets var->expr* body)
  (match var->expr*
    ['() body]
    [`((,var . ,expr) . ,d)
      (Let var expr (make-lets d body))]))

(define (rco-exp e)
  (match e
    [(Int i) (Int i)]
    [(Var v) (Var v)]
    [(Bool b) (Bool b)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim op `(,e)) #:when (member op '(not -))
     (let-values ([(atm atm->expr) (rco-atm e)])
       (let ([atm (if (symbol? atm) (Var atm) atm)])
         (make-lets atm->expr (Prim op `(,atm)))))]
    [(Prim op `(,e1 ,e2)) #:when (member op '(eq? < +))
     (let-values ([(atm1 atm->subexpr1) (rco-atm e1)]
                  [(atm2 atm->subexpr2) (rco-atm e2)])
       (let ([atm1 (if (symbol? atm1) (Var atm1) atm1)]
             [atm2 (if (symbol? atm2) (Var atm2) atm2)])
         (make-lets (append atm->subexpr1 atm->subexpr2) (Prim op `(,atm1 ,atm2)))))]
    [(If c t e)
     (If (rco-exp c) (rco-exp t) (rco-exp e))]
    [(Let x v b) (Let x (rco-exp v) (rco-exp b))]))

;; remove-complex-opera* : R1 -> R1
;; arguments of operations are atomic
(define (remove-complex-opera* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))

; c1 : Assign, Assign, ..., Ret
; c2 : Assign, Assign, ..., Ret
(define (merge-conts c1 c2 x)
  (match c1
    [(Return v)
     (Seq (Assign (Var x) v) c2)]
    [(Seq a tail)
     (Seq a (merge-conts tail c2 x))]
    [else (error "Couldn't merge : " c1 c2)]))

(define (block->goto b label->block)
  (delay
    (let ([b (force b)])
      (match b
        [(Goto l) (Goto l)]
        [else (let ([l (gensym 'block)])
                (dict-set! label->block l b)
                (Goto l))]))))

; explicate-pred (if e1 e2 e2) B1 B2 => B5
; add B1 and B2 to CFG with labels l1 and l2
; (explicate-pred e2 (goto l1) (goto l2)) => B3
; (explicate-pred e3 (goto l1) (goto l2)) => B4
; (explicate-pred e1 B3 B4) => B5
; TODO : add following test
; (if (if (if #t #f #t) #f #t) )
; whe t and e are forced, the generated blocks which are added to label->block
; and then goto for that block is inserted
(define (explicate-pred c t e label->block)
  (match c
    [(Var x) (IfStmt (Prim 'eq? `(,(Var x) ,(Bool #t)))
                     (force (block->goto t label->block))
                     (force (block->goto e label->block)))]
    [(Bool #t) (force (block->goto t label->block))]
    [(Bool #f) (force (block->goto e label->block))]
    [(Prim op es) #:when (or (eq? op 'eq?) (eq? op '<))
     (IfStmt (Prim op es)
             (force (block->goto t label->block))
             (force (block->goto e label->block)))]
    [(Prim 'not `(,e1))
     (explicate-pred e1 e t label->block)]
    [(Let x rhs body)
     (let ([cont (delay (explicate-pred body t e label->block))])
       (explicate-assign rhs x cont label->block))]
    [(If c1 t1 e1)
     (let([t2 (delay (explicate-pred t1 t e label->block))]
          [e2 (delay (explicate-pred e1 t e label->block))])
       (explicate-pred c1 t2 e2 label->block))]
    [else (error "explicate-pred passed non bool type expr : " c)]))

(define (explicate-assign e x cont label->block)
  (match e
    [(Bool b) (Seq (Assign (Var x) (Bool b)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Let y rhs body)
     ; TODO : try passing cont to explicate-tail to avoid merge-conts
     (let* ([cont1 (explicate-tail body label->block)]
            [cont (merge-conts cont1 cont x)])
       (explicate-assign rhs y cont label->block))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) (force (block->goto cont label->block)))]
    [(If c t e)
     (let* ([cont-t (delay (explicate-assign t x cont label->block))]
            [cont-e (delay (explicate-assign e x cont label->block))])
       (explicate-pred c cont-t cont-e label->block))]
    [else (error "explicate-assign unhandled case" e)]))

(define (explicate-tail e label->block)
  (match e
    [(Var x) (Return (Var x))]
    [(Bool b) (Return (Bool b))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body)
     (let* ([cont (explicate-tail body label->block)])
       (explicate-assign rhs x cont label->block))]
    [(Prim op es) (Return (Prim op es))]
    [(If c t e)
     (let ([t1 (block->goto (delay (explicate-tail t label->block)) label->block)]
           [e1 (block->goto (delay (explicate-tail e label->block)) label->block)])
     (explicate-pred c t1 e1 label->block))]
    [else (error "explicate-tail was passed a non tail expression : " e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body)
     (let* ([label->block (make-hash)]
            [tail (explicate-tail body label->block)]
            [_ (dict-set! label->block 'start tail)])
       (CProgram info (hash->list label->block)))]))

(define (si-atm e)
  (match e
    [(Int n) (Imm n)]
    [(Var v) (Var v)]
    [(Bool #f) (Imm 0)]
    [(Bool #t) (Imm 1)]
    [else (error "si-atm passed non-atom expression : " e)]))

(define (si-stmt e)
  (match e
    [(Assign (Var v) (Int i)) `(,(Instr 'movq `(,(Imm i) ,(Var v))))]
    [(Assign (Var v) (Bool b)) `(,(Instr 'movq `(,(si-atm (Bool b)) ,(Var v))))]
    [(Assign (Var v) (Var u)) `(,(Instr 'movq `(,(Var u) ,(Var v))))]
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
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'cmpq `(,a2 ,a1))
         ,(Instr 'set `(e ,(Reg 'al)))
         ,(Instr 'movzbq `(,(Reg 'al) ,(Var v)))))]
    [(Assign (Var v) (Prim '< `(,a1 ,a2)))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'cmpq `(,a2 ,a1))
         ,(Instr 'set `(l ,(Reg 'al)))
         ,(Instr 'movzbq `(,(Reg 'al) ,(Var v)))))]
    [else (error "si-stmt passed non-statement expression : " e)]))

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
    [(Seq stmt tail)
     (let ([s (si-stmt stmt)]
           [t (si-tail tail)])
       (append s t))]
    [(Goto l) `(,(Jmp l))]
    [(IfStmt (Prim 'eq? `(,a1 ,a2)) (Goto l1) (Goto l2))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'cmpq `(,a2 ,a1))
         ,(JmpIf 'e l1)
         ,(Jmp l2)))]
    [(IfStmt (Prim '< `(,a1 ,a2)) (Goto l1) (Goto l2))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'cmpq `(,a2 ,a1))
         ,(JmpIf 'l l1)
         ,(Jmp l2)))]
    [else (error "si-tail passed non-tail expression : " e)]))

;; select-instructions : c0 -> [pseudo-x86]
(define (select-instructions p)
  (match p
    [(CProgram info label-tails)
     (let* ([label-blocks (map (lambda (label-tail)
                                 `(,(car label-tail) . ,(Block '() (si-tail (cdr label-tail))))) label-tails)])
       (X86Program info label-blocks))]))

(define ((ah-arg var->stk) atm)
  (match atm
    [(or (Reg _) (Imm _)) atm]
    [(Var v)
     (let ([p (assv v var->stk)])
       (if p
         (Deref 'rbp (cdr p))
         (error "var ~s not allocated on stack : ~s\n" v var->stk)))]
    [else (error "ah-arg passed non-atm expr : " atm)]))

(define ((ah-instr var->stk) instr)
  (match instr
    [(Instr op args)
     (let ([args (map (ah-arg var->stk) args)])
       (Instr op args))]
    [_ instr]
    [else (error "ah-instr passed non-instr expression : " instr)]))

(define ((ah-block var->stk) blk)
  (match blk
    [(Block info instrs)
     (let ([instrs (map (ah-instr var->stk) instrs)])
       (Block info instrs))]))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program info label-blocks)
     (let* ([locals (assv 'locals info)]
            [locals (if locals
                      (cdr locals)
                      '())]
            [var->stk (foldr (lambda (var acc)
                               (cons `(,var . ,(* -8 (+ 1 (index-of locals var)))) acc))
                             '()
                             locals)]
            [label-blocks (map (lambda (label-block)
                                 (cons (car label-block) ((ah-block var->stk) (cdr label-block))))
                               label-blocks)]
            [stack-space (if (empty? var->stk)
                           0
                           (round-frame (- (cdr (last var->stk)))))])
       (X86Program (dict-set info 'stack-space stack-space) label-blocks))]))

(define (instr-w instr)
  (match instr
    [(Instr _ `(,_ ,a2)) (ul-arg a2)]
    [(Instr 'negq `(,arg)) (ul-arg arg)]
    [(Callq _ _) (list->set caller-saved)]
    [else (set)]))

(define (instr-r instr)
  (match instr
    [(Instr (or 'addq 'subq) `(,a1 ,a2)) (set-union (ul-arg a1) (ul-arg a2))]
    [(Instr 'movq `(,a1 ,_)) (ul-arg a1)]
    [(Instr 'negq `(,arg)) (ul-arg arg)]
    [(Callq _ i) (list->set (take callee-saved i))]
    [else (set)]))

(define (ul-arg a)
  (match a
    [(or (Var _) (Reg _)) (set a)]
    [_ (set)]))

; Lafter(n) = {}
; Lafter(k) =  Lbefore(k - 1)
; Lbefore(k) = (Lafter(k) - W(k)) U R(k)
; Thus, Lafter(k) = Lafter(k + 1) - W(k) U R(k)
(define (ul-instrs instrs after label->live)
  (match instrs
    ['() after]
    [`(,(Jmp label) . ,t)
      (let ([a (if (dict-has-key? label->live label)
                 (dict-ref label->live label)
                 (set))])
        (ul-instrs t (cons (set-union a (car after)) after) label->live))]
    [`(,i . ,t)
      (let ([w (instr-w i)]
            [r (instr-r i)]
            [a (car after)])
        (ul-instrs t (cons (set-union (set-subtract a w) r) after) label->live))]))

(define (ul-block blk)
  (match blk
    [(Block info instrs)
     (let* ([r (reverse instrs)]
            ; XXX : ugly hardcoded label->live
            [live-after
              (ul-instrs r `(,(set)) `((conclusion . ,(set (Reg 'rax) (Reg 'rsp)))))])
       (Block (dict-set* info 'live-after live-after 'label->live `((conclusion . ,(set 'rax 'rsp))))
              instrs))]))

(define (uncover-live p)
  (match p
    [(X86Program info label-blocks)
     (let ([label-blocks (map (lambda (label-block)
                                (cons (car label-block)
                                      (ul-block (cdr label-block))))
                              label-blocks)])
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

(define (bi-block blk)
  (match blk
    [(Block info instrs)
     (let* ([live-after (dict-ref info 'live-after)]
            [vertices (apply set-union live-after)]
            [g (undirected-graph '())]
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
     (let* ([start-block (dict-ref label-blocks 'start)]
            [g (bi-block start-block)])
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
                                          (dict-values var->mem))])
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
    [else (error "print-x86-arg passed non-arg expr : " arg)]))

(define (print-x86-args args)
  (match args
    [`(,a) (print-x86-arg a)]
    [`(,a1 ,a2)
      (string-append (print-x86-arg a1) ", " (print-x86-arg a2))]))

(define (print-x86-instr instr)
  (match instr
    [(Instr op args)
     (string-append (symbol->string op) " "
                    (print-x86-args args))]
    [(Callq l i) (string-append "callq "
                                (symbol->string l))]
    [(Retq) "retq"]
    [(Jmp l) (string-append "jmp " (symbol->string l) "\n")]))

(define (print-x86-block blk)
  (match blk
    [(Block info instrs)
     (foldr (lambda (instr acc)
              (string-append "\t" (print-x86-instr instr) "\n" acc))
            ""
            instrs)]
    [_ (error "print-x86-block passed non block value : " blk)]))

(define (print-main-block stack-space used-callee)
  (string-append "main:\n"
                 "\tpushq %rbp\n"
                 "\tmovq %rsp, %rbp\n"
                 (apply string-append
                        (map (lambda (r)
                               (string-append "\tpushq %"
                                              (symbol->string (Reg-name r))
                                              "\n"))
                             used-callee))
                 "\tsubq $" (number->string (round-frame (+ (* 8 (length used-callee)) stack-space))) ", %rsp\n"
                 "\tjmp start\n"))

(define (print-conclusion-block stack-space used-callee)
  (string-append "conclusion:\n"
                 (apply string-append
                        (map (lambda (r)
                               (string-append "\tpopq %"
                                              (symbol->string (Reg-name r))
                                              "\n"))
                             used-callee))
                 "\taddq $" (number->string (round-frame (+ (* 8 (length used-callee)) stack-space))) ", %rsp\n"
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

#|
(define v->m (list (cons (Var 'x18487) (Reg 'rbx)) (cons (Var 'x18488) (Reg 'rax))))

(define b
  (Block '()
         `(,(Instr 'movq `(,(Imm 1) ,(Var 'v)))
            ,(Instr 'movq `(,(Imm 42) ,(Var 'w)))
            ,(Instr 'movq `(,(Var 'v) ,(Var 'x)))
            ,(Instr 'addq `(,(Imm 7) ,(Var 'x)))
            ,(Instr 'movq `(,(Var 'x) ,(Var 'y)))
            ,(Instr 'movq `(,(Var 'x) ,(Var 'z)))
            ,(Instr 'addq `(,(Var 'w) ,(Var 'z)))
            ,(Instr 'movq `(,(Var 'y) ,(Var 't)))
            ,(Instr 'negq `(,(Var 't)))
            ,(Instr 'movq `(,(Var 'z) ,(Reg 'rax)))
            ,(Instr 'movq `(,(Var 't) ,(Reg 'rax)))
            ,(Jmp 'conclusion))))

(define b1
  (Block
    '()
    (list
     (Instr 'movq (list (Imm 20) (Var 'x18487)))
     (Instr 'movq (list (Imm 0) (Var 'x18488)))
     (Instr 'addq (list (Imm 22) (Var 'x18488)))
     (Instr 'movq (list (Var 'x18487) (Reg 'rax)))
     (Instr 'addq (list (Var 'x18488) (Reg 'rax)))
     (Jmp 'conclusion))))

(define (color-block b)
  (let* ([g (bi-block (ul-block b))]
         [w (filter Var? (get-vertices g))]
         [v->s (make-hash (foldr (lambda (x a)
                                   (cons `(,x . ,(set)) a)) '() w))]
         [q (make-pqueue (lambda (x1 x2)
                           (> (set-count (dict-ref v->s x1)) (set-count (dict-ref v->s x2)))))]
         [vertex->node (map (lambda (v) (cons v (pqueue-push! q v))) w)])
    (color-graph g q w v->s vertex->node)))

|#
