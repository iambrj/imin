#lang racket

(require "utilities.rkt")

(provide (all-defined-out))

(define caller-saved
  (let ([r '(rax rcx rdx rsi rdi r8 r9 r10 r11)])
    (map Reg r)))
(define callee-saved
  (let ([r '(rsp rbp rbx r12 r13 r14 r15)])
    (map Reg r)))
(define param-reg*
  (let ([r '(rdi rsi rdx rcx r8 r9)])
    (map Reg r)))
#|
Register conventions
rax -> temporary moves to tackle movqs with both stack args
r11 -> vector-set, vector-ref
r15 -> shadow stack top
|#
(define usable-registers
  (let ([r '(rbx rcx rdx rsi rdi r8 r9 r10 r12 r13 r14)])
    (map Reg r)))

(define shadow-stk-reg 'r15)
(define stk-reg 'rbp)

