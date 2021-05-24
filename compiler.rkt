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

(define caller-saved '(rax rcx rdx rsi rdi r8 r9 r10 r11))
(define callee-saved '(rsp rbp rbx r12 r13 r14 r15))
(define arg-regs (list->set '(rdi rsi rdx rcx r8 r9)))
(define usable-registers (list->set '(rax rcx rdx rsi rdi r8 r9 r10 r11 rbx r12 r13 r14 r15)))

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

; uniquify-exp -> remove-complex-opera* -> explicate-control ->
; select-instructions -> assign-homes -> patch-instructions -> print x86

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

(define (merge-conts c1 c2 x)
  (match c1
    [(Return v)
     (Seq (Assign (Var x) v) c2)]
    [(Seq a tail)
     (Seq a (merge-conts tail c2 x))]
    [else (error "Couldn't merge : " c1 c2)]))

(define (explicate-tail e vars)
  (match e
    [(Var x)
     (let ([vars (if (not (member x vars))
                   (cons x vars)
                   vars)])
       (values vars
               (Return (Var x))))]
    [(Int n)
     (values vars
             (Return (Int n)))]
    [(Let x rhs body)
     (let*-values ([(vars cont) (explicate-tail body vars)]
                   [(vars) (if (not (member x vars))
                             (cons x vars)
                             vars)])
       (explicate-assign rhs x cont vars))]
    [(Prim op es)
     (values vars
             (Return (Prim op es)))]
    [else (error "explicate-tail was passed a non tail expression : " e)]))

(define (explicate-assign e x cont vars)
  (match e
    [(Var y)
     (let ([vars (if (not (member y vars))
                   (cons y vars)
                   vars)])
       (values vars
               (Seq (Assign (Var x) (Var y)) cont)))]
    [(Int n)
     (values vars
             (Seq (Assign (Var x) (Int n)) cont))]
    [(Let y rhs body)
     (let*-values ([(vars cont1) (explicate-tail body vars)]
                   [(cont) (merge-conts cont1 cont x)]
                   [(vars) (if (not (member y vars))
                             (cons y vars)
                             vars)])
       (explicate-assign rhs y cont vars))]
    [(Prim op es)
     (values vars
             (Seq (Assign (Var x) e) cont))]
    [else (error "explicate-tail unhandled case" e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body)
     (let-values ([(vars tail) (explicate-tail body '())])
       (CProgram `((locals . ,vars)) `((start . ,tail))))]))

(define (si-atm e)
  (match e
    [(Int n) (Imm n)]
    [(Var v) (Var v)]
    [else (error "si-atm passed non-atom expression : " e)]))

(define (si-stmt e)
  (match e
    [(Assign (Var v) (Int i)) `(,(Instr 'movq `(,(Imm i) ,(Var v))))]
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
         [(equal? (Var v) a1) `(,(Instr 'addq `(,(si-atm a1) ,(Var v))))]
         [(equal? (Var v) a2) `(,(Instr 'addq `(,(si-atm a2) ,(Var v))))]
         [else `(,(Instr 'movq `(,a1 ,(Var v))) ,(Instr 'addq `(,a2 ,(Var v))))]))]
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
       (X86Program (cons`(stack-space . ,stack-space) info) label-blocks))]))

(define (pi-instr instr)
  (match instr
    [(Instr op args)
     (match args
       [`(,(Deref 'rbp x) ,(Deref 'rbp y))
         `(,(Instr 'movq `(,(Deref 'rbp x) ,(Reg 'rax)))
            ,(Instr op `(,(Reg 'rax) ,(Deref 'rbp y))))]
       [_ `(,instr)])]
    [_ `(,instr)]))

(define (pi-block blk)
  (match blk
    [(Block info instrs)
     (let ([instrs (foldl (lambda (instr acc)
                            (append acc (pi-instr instr)))
                          '()
                          instrs)])
       (Block info instrs))]))

;; patch-instructions : psuedo-x86 -> x86
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
    [(Callq l i) (symbol->string l)]
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

(define (print-main-block stack-space)
  (string-append "main:\n"
                 "\tpushq %rbp\n"
                 "\tmovq %rsp, %rbp\n"
                 "\tsubq $" (number->string stack-space) ", %rsp\n"
                 "\tjmp start\n"))

(define (print-conclusion-block stack-space)
  (string-append "conclusion:\n"
                 "\taddq $" (number->string stack-space) ", %rsp\n"
                 "\tpopq %rbp\n"
                 "\tretq\n"))

;; print-x86 : x86 -> string
(define (print-x86 p)
  (match p
    [(X86Program info label-blocks)
     (let* ([stack-space (assv 'stack-space info)]
            [stack-space (if stack-space
                           (cdr stack-space)
                           0)]
            [main (print-main-block stack-space)]
            [conclusion (print-conclusion-block stack-space)]
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

; Lafter(k) = Lafter(k + 1) - W(k) U R(k)
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
     (let ([_ (map (lambda (v)
                     (if (or (equal? v d) (equal? v s))
                       (void)
                       (add-edge! g d v)))
                   (set->list live-after))])
       g)]
    [else (let* ([w (instr-w instr)]
                 [_ (for* ([d w]
                           [v live-after])
                      (if (equal? v d)
                        (void)
                        (add-edge! g d v)))])
            g)]))

(define (bi-block blk)
  (match blk
    [(Block info instrs)
     (let* ([live-after (dict-ref info 'live-after)]
            [vertices (apply set-union live-after)]
            [g (undirected-graph '())]
            [_ (for ([v (set->list vertices)]) (add-vertex! g v))]
            [_ (for ([i instrs] [la live-after]) (bi-instr i la g))])
       (Block (dict-set info 'conflicts g) instrs))]))

(define (build-interference p)
  (match p
    [(X86Program info label-blocks)
     (let ([label-blocks (map (lambda (label-block)
                                (cons (car label-block)
                                      (bi-block (cdr label-block))))
                              label-blocks)])
       (X86Program info label-blocks))]))

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

(define (update-neighbors vertex->color color neighbors)
  (match neighbors
    [`() vertex->color]
    [`(,n . ,d)
      (let* ([s (dict-ref vertex->color n)]
             [vertex->color (dict-set vertex->color n (set-union s (set color)))])
        (update-neighbors vertex->color color d))]))

(define (color-graph g q w vertex->satur [vertex->color '()])
  (match w
    ['() vertex->color]
    [else (let* ([v (car (pqueue-pop! q))]
                 [w (set-remove w v)]
                 [satur (dict-ref vertex->satur v)]
                 [color (min-free satur)]
                 [neighbors (filter Var? (get-neighbors g v))]
                 [_ (foldr (lambda (x a)
                             (pqueue-decrease-key! q (cons x (set-count (dict-ref vertex->satur x)))))
                           '()
                           (set->list w))]
                 [vertex->satur (update-neighbors vertex->satur color neighbors)])
            (color-graph g q w vertex->satur vertex->color))]))
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

#|
(define g
  (let ([b (bi-block (ul-block b))])
    (match b
      [(Block info instrs)
       (dict-ref info 'conflicts)])))

(define w (filter Var? (get-vertices g)))

(define q
  (let ([q (make-pqueue (lambda (x1 x2)
                          (> (cdr x1) (cdr x2))))])
    (begin (foldr
             (lambda (x a)
               (pqueue-push! q (cons x 0)))
             q
             w)
           q)))

(define v->s
  (foldr (lambda (x a)
           `((,x . ,(set)) . ,a))
         '()
         w))

(printf "w : ~s\n" w)
(printf "v->s : ~s\n" v->s)
(printf "q : ~s\n" q)

(color-graph g q w v->s)
|#
