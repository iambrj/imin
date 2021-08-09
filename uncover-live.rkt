#lang racket

(require "utilities.rkt"
         "utils.rkt"
         graph)

(provide uncover-live)

(define (uncover-live p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info (map ul-def def*))]))

(define (ul-def d)
  (match d
    [(Def name param* rty info label-block*)
     (let* ([cfg (dict-ref info 'cfg)]
            ; conclusion is always last, remove it since not yet generated
            ; XXX : Hardcoded conclusion block naming convention
            [labels (filter (compose not conclusion?) (tsort (transpose cfg)))]
            [c (string->symbol (string-append (symbol->string name)
                                              "conclusion"))]
            [label->liveafter (make-hash `((,c . ,(set (Reg 'rax) (Reg 'rsp)))))]
            [label-block* (map (lambda (label)
                                 (ul-block label
                                           (dict-ref label-block* label)
                                           label->liveafter))
                               labels)])
       (Def name param* rty info label-block*))]))

(define (conclusion? c)
  (let ([c (symbol->string c)])
    (string-suffix? c "conclusion")))

(define (ul-block l blk label->liveafter)
  (match blk
    [(Block info instrs)
     (let* ([liveafter (ul-instrs (reverse instrs) `(,(set)) label->liveafter)]
            [_ (dict-set! label->liveafter l (car liveafter))])
       (cons l (Block (dict-set info 'live-after liveafter) instrs)))]
    [else (error "ul-block unhandled case " blk)]))

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
