#lang racket

(require "utilities.rkt")

(provide print-x86)

(define (print-x86 p)
  (match p
    [(ProgramDefs info def*)
     (let ([l (map print-x86-def def*)])
       (apply string-append l))]))

(define (print-x86-def p)
  (match p
    [(Def name param* rty info label-block*)
     (let* ([stack-space (dict-ref info 'stack-space)]
            [shadow-stack-space (dict-ref info 'shadow-stack-space)]
            [used-callee (dict-ref info 'used-callee)]
            [prelude (print-x86-prelude name stack-space shadow-stack-space used-callee)]
            [conclusion (print-conclusion-block name stack-space shadow-stack-space used-callee)]
            [label-block* (let ([l (for/list ([label-block label-block*])
                                     (string-append (format "~a:\n" (car label-block))
                                                    (print-x86-block (cdr label-block)
                                                                     stack-space
                                                                     shadow-stack-space
                                                                     used-callee)))])
                            (apply string-append l))])
       (string-append prelude label-block* conclusion))]))

(define (print-x86-block blk stack-space shadow-stack-space used-callee)
  (match blk
    [(Block info instr*)
     (let ([instr* (for/list ([instr instr*])
                     (format "\t~a\n" (print-x86-instr stack-space
                                                       shadow-stack-space
                                                       used-callee
                                                       instr)))])
       (apply string-append instr*))]
    [_ (error "print-x86-block unhandled case " blk)]))

(define (print-x86-instr stack-space shadow-stack-space used-callee instr)
  (match instr
    [(Callq l i) (format "callq ~a" l)]
    [(Retq) "retq"]
    [(Jmp l) (format "jmp ~a" l)]
    [(JmpIf cc l) (format "j~a ~a" cc l)]
    [(Instr 'set `(,cc ,r)) (format "set~a ~a" cc r)]
    [(Instr op args)
     (string-append (format "~a " op)
                    (print-x86-args args))]
    [(IndirectCallq arg len) (format "callq *~a" (print-x86-arg arg))]
    [(TailJmp arg len) 
     (string-append (print-restore stack-space shadow-stack-space used-callee)
                    (format "\tjmp *~a" (print-x86-arg arg)))]))

(define (print-x86-args args)
  (match args
    [`(,a) (print-x86-arg a)]
    [`(,a1 ,a2) (format "~a, ~a" (print-x86-arg a1) (print-x86-arg a2))]))

(define (print-x86-arg arg)
  (match arg
    [(Global x) (format "~a(%rip)" x)]
    [(Imm i) (format "$~a" i)]
    [(Reg r) (format "%~a" r)]
    [(Deref r i) (format "~a(%~a)" i r)]
    [(FunRef l) (format "~a(%rip)" l)]))

(define (print-x86-prelude name stack-space shadow-stack-space used-callee)
  (string-append (print-directives name)
                 (format "~a:\n" name)
                 "\tpushq %rbp\n"
                 "\tmovq %rsp, %rbp\n"
                 (print-pushes used-callee)
                 (print-alignment-setup stack-space used-callee)
                 (if (eqv? name 'main)
                   print-gc-init
                   "")
                 (print-gc-setup shadow-stack-space)
                 (format "\tjmp ~astart\n" name)))

(define (print-alignment op stack-space used-callee)
  (let* ([memory (+ stack-space
                    (* 8 (length used-callee))
                    8)] ; rbp
         [arg (if (odd? (modulo memory 8))
                stack-space
                (+ 8 stack-space))])
    (if (> arg 0)
      (format "\t~a $~a, %rsp\n" op arg)
      "")))

(define (print-alignment-setup stack-space used-callee)
  (print-alignment "subq" stack-space used-callee))

(define (print-directives name)
  (string-append (format "\t.globl ~a\n" name)
                 "\t.align 16\n"))

(define print-gc-init
  (string-append
    "\tmovq $16384, %rdi\n"
    "\tmovq $16384, %rsi\n"
    "\tcallq initialize\n"
    "\tmovq rootstack_begin(%rip), %r15\n"))

(define (print-gc-setup shadow-stack-space)
  (string-append "\tmovq $0, %r15\n"
                 (if (> shadow-stack-space 0)
                   (format "\taddq $~a, %r15\n" shadow-stack-space)
                   "")))

(define (print-pushes used-callee)
  (let ([l (for/list ([r used-callee])
             (format "\tpushq %~a\n" (Reg-name r)))])
    (apply string-append l)))

(define (print-conclusion-block name stack-space shadow-stack-space used-callee)
  (string-append (format "~aconclusion:\n" name)
                 (print-restore stack-space shadow-stack-space used-callee)
                 "\tretq\n"))

(define (print-restore stack-space shadow-stack-space used-callee)
  (string-append (print-gc-restore shadow-stack-space)
                 (print-alignment-restore stack-space used-callee)
                 (print-pops used-callee)
                 "\tpopq %rbp\n"))

(define (print-gc-restore shadow-stack-space)
  (if (> shadow-stack-space 0)
    (format "\tsubq $~a, %r15\n" shadow-stack-space)
    ""))

(define (print-alignment-restore stack-space used-callee)
  (print-alignment "addq" stack-space used-callee))

(define (print-pops used-callee)
  (let ([l (for/list ([r used-callee])
             (format "\tpopq %~a\n" (Reg-name r)))])
    (apply string-append l)))
