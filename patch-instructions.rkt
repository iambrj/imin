#lang racket

(require "utilities.rkt")

(provide patch-instructions)

(define (trivial-mov instr)
  (match instr
    [(Instr 'movq `(,a ,a)) #t]
    [_ #f]))

(define (pi-instr instr)
  (match instr
    [(Instr op `(,(Deref s x) ,(Deref d y)))
     `(,(Instr 'movq `(,(Deref s x) ,(Reg 'rax)))
        ,(Instr op `(,(Reg 'rax) ,(Deref d y))))]
    ; [TODO proof] Invariant: this move into rax will not mess with other uses
    ; of rax
    [(TailJmp r l) #:when (not (Reg? r))
     `(,(Instr 'movq `(,r ,(Reg 'rax)))
        ,(TailJmp (Reg 'rax) l))]
    ; [TODO proof] Invariant: this move into rax will not mess with other uses
    ; of rax
    [(Instr 'leaq `(,s ,d)) #:when (not (Reg? d))
     `(,(Instr 'leaq `(,s ,(Reg 'rax)))
        ,(Instr 'movq `(,(Reg 'rax) ,d)))]
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

; 1. Remove trivial moves e.g. mov rax rax
; 2. Remove multi memory accesses in same instruction e.g. transform mov -8rbp,
; -16rbp to mov -8rbp, rax + mov rax, -16rbp
(define (pi-def d)
  (match d
    [(Def name param* rty info label-block*)
     (let ([label-block* (map (lambda (label-block)
                                (cons (car label-block)
                                      (pi-block (cdr label-block))))
                              label-block*)])
       (Def name param* rty info label-block*))]))

(define (patch-instructions d)
  (match d
    [(ProgramDefs info def*)
     (ProgramDefs info (map pi-def def*))]))
