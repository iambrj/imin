#|
author : brj
lives at : https://github.com/iambrj/imin
|#

#lang racket
(require racket/set
         racket/stream
         racket/fixnum
         graph
         "interp-Rint.rkt"
         "utilities.rkt"
         "utils.rkt"
         "constants.rkt"
         "shrink.rkt"
         "reveal-functions.rkt"
         "limit-functions.rkt"
         "expose-allocation.rkt"
         "uniquify.rkt"
         "rco.rkt"
         "explicate-control.rkt"
         "select-instructions.rkt"
         "build-cfg.rkt"
         "uncover-live.rkt"
         "build-interference.rkt"
         "allocate-registers.rkt"
         "patch-instructions.rkt"
         "print-x86.rkt")

(provide shrink
         reveal-functions
         limit-functions
         expose-allocation
         uniquify
         remove-complex-opera*
         explicate-control
         select-instructions
         build-cfg
         uncover-live
         build-interference
         allocate-registers
         patch-instructions
         print-x86)
