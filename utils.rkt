#lang racket

(require "utilities.rkt"
         "constants.rkt")

(provide instr-w
         instr-r
         param-name
         atom?
         build-lets)

; XXX : hardocded to al, will have to change if other registers are used
(define (instr-w instr)
  (match instr
    [(Instr i `(,_ ,a2)) #:when (member i '(movq addq subq xorq movzbq set leaq))
     (filter-reg-var a2)]
    [(Instr 'negq `(,arg)) (filter-reg-var arg)]
    [(or (Jmp _) (JmpIf _ _) (Instr 'cmpq _)) (set)]
    [(or (TailJmp _ _) (IndirectCallq _ _) (Callq _ _)) (list->set caller-saved)]
    [else (error "instr-w missing case : " instr)]))

(define (instr-r instr)
  (match instr
    [(Instr i `(,a1 ,a2)) #:when (member i '(addq subq xorq cmpq))
     (set-union (filter-reg-var a1) (filter-reg-var a2))]
    [(Instr i `(,a1 ,_)) #:when (member i '(movq subq movzbq))
     (filter-reg-var a1)]
    [(Instr 'negq `(,arg)) (filter-reg-var arg)]
    [(or (TailJmp a i) (IndirectCallq a i)) (set-union (filter-reg-var a) (list->set (take callee-saved i)))]
    [(Callq _ i) (list->set (take callee-saved i))]
    [(or (Instr 'leaq _) (Instr 'set _)) (set)]
    [else (error "instr-r missing case : " instr)]))

(define (filter-reg-var a)
  (match a
    [(Reg 'al) (Reg 'rax)]
    [(or (Var _) (Reg _)) (set a)]
    [(Deref r _) (set (Reg r))]
    [(or (Global _) (Bool _) (Int _) (Imm _) (FunRef _)) (set)]))

(define (param-name p)
  (match p
    [`(,x : ,_) x]
    [else (error "param-name passed non-param : " p)]))

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
