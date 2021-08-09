#lang racket

(require "utilities.rkt"
         "constants.rkt"
         "utils.rkt")

(provide select-instructions)

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
    [(Assign (Var v) (FunRef f))
     `(,(Instr 'leaq `(,(FunRef f) ,(Var v))))]
    [(Assign (Var v) (Call fun arg*))
     (let ([movs (map (lambda (idx)
                        (Instr 'movq `(,(si-atm (list-ref arg* idx))
                                        ,(list-ref param-reg* idx))))
                      (range 0 (length arg*)))])
       (append movs
               `(,(IndirectCallq fun (length arg*))
                  ,(Instr 'movq `(,(Reg 'rax) ,(Var v))))))]
    [else (error "si-stmt unhandled case : " e)]))

(define (si-tail e c)
  (match e
    [(Return (Var v))  `(,(Instr 'movq `(,(Var v) ,(Reg 'rax))) ,(Jmp c))]
    [(Return (Int i))  `(,(Instr 'movq `(,(Imm i) ,(Reg 'rax))) ,(Jmp c))]
    [(Return (Prim 'read '()))
     `(,(Callq 'read_int 0)
        ,(Jmp c))]
    [(Return (Prim '- `(,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,a ,(Reg 'rax)))
          ,(Instr 'negq `(,(Reg 'rax)))
          ,(Jmp c)))]
    [(Return (Prim '+ `(,a1 ,a2)))
     (let ([a1 (si-atm a1)]
           [a2 (si-atm a2)])
       `(,(Instr 'movq `(,a1 ,(Reg 'rax)))
          ,(Instr 'addq `(,a2 ,(Reg 'rax)))
          ,(Jmp c)))]
    [(Return (Prim 'vector-ref `(,v ,(Int n))))
     `(,(Instr 'movq `(,v ,(Reg 'r11)))
        ,(Instr 'movq `(,(Deref 'r11 (* 8 (+ n 1))) ,(Reg 'rax)))
        ,(Jmp c))]
    [(Return (Prim 'vector-set! `(,v ,(Int n) ,a)))
     (let ([a (si-atm a)])
       `(,(Instr 'movq `(,v ,(Reg 'r11)))
          ,(Instr 'movq `(,a ,(Deref 'r11 (* 8 (+ n 1)))))
          ,(Instr 'movq `(,(Imm 0) ,(Reg 'rax)))
          ,(Jmp c)))]
    [(Seq stmt tail)
     (let ([s (si-stmt stmt)]
           [t (si-tail tail c)])
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
    [(TailCall fun arg*)
     (let ([movs (map (lambda (idx)
                        (Instr 'movq `(,(si-atm (list-ref arg* idx))
                                        ,(list-ref param-reg* idx))))
                      (range 0 (length arg*)))])
       (append movs
               `(,(TailJmp fun (length arg*)))))]
    [else (error "si-tail unhandled case : " e)]))

(define (si-def d)
  (match d
    [(Def name param* rty info label-tail*)
     (let* ([param* (map param-name param*)]
            [arg-mov* (for/list ([idx (in-range (length param*))])
                        (Instr 'movq `(,(list-ref param-reg* idx)
                                        ,(Var (list-ref param* idx)))))]
            [start-label (gen-start-label name)]
            [concl-label (gen-concl-label name)]
            [start `(,start-label
                      . ,(Block '() (append arg-mov*
                                            (si-tail (dict-ref label-tail*
                                                               start-label)
                                                     concl-label))))]
            [rest (for/list ([label-tail label-tail*])
                    (let ([label (car label-tail)]
                          [tail (cdr label-tail)])
                      `(,label . ,(Block '() (si-tail tail concl-label)))))])
       (Def name param* 'Integer `((num-params . ,(length param*)) . ,info)
            `(,start . ,rest)))]))

(define (gen-start-label name)
  (string->symbol (string-append (symbol->string name)
                                 "start")))

(define (gen-concl-label name)
  (string->symbol (string-append (symbol->string name)
                                 "conclusion")))

; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info (map si-def def*))]))
