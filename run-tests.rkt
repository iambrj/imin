#! /usr/bin/env racket
#lang racket

(require "utilities.rkt"
         "interp-Rfun.rkt"
         "interp-Rfun-prime.rkt"
         "type-check-Rfun.rkt"
         "interp-Cfun.rkt"
         "type-check-Cfun.rkt"
         "interp.rkt"
         "compiler.rkt")
;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; should be named "compiler.rkt"
(define passes
  `(("shrink" ,shrink ,interp-Rfun ,type-check-Rfun)
    ("reveal functions" ,reveal-functions ,interp-Rfun-prime ,type-check-Rfun)
    ("limit functions" ,limit-functions ,interp-Rfun-prime ,type-check-Rfun)
    ("expose allocation" ,expose-allocation ,interp-Rfun-prime ,type-check-Rfun)
    ("uniquify" ,uniquify ,interp-Rfun-prime ,type-check-Rfun)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Rfun-prime ,type-check-Rfun)
    ("explicate control" ,explicate-control ,interp-Cfun ,type-check-Cfun)
    ("select instructions" ,select-instructions ,interp-pseudo-x86-3)
    ("build cfg" ,build-cfg ,interp-pseudo-x86-3)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-3)
    ("build interference" ,build-interference ,interp-pseudo-x86-3)
    #;("allocate registers" ,allocate-registers ,interp-pseudo-x86-3)
    #;("patch instructions" ,patch-instructions ,interp-pseudo-x86-3)
    #;("print x86" ,print-x86 #f)))

#;(define passes
  `(("shrink" ,shrink ,interp-Rvec ,type-check-Rvec)
    ("expose allocation" ,expose-allocation ,interp-Rvec-prime ,type-check-Rvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Rvec-prime ,type-check-Rvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ("select instructions" ,select-instructions ,interp-pseudo-x86-2)
    ("build cfg" ,build-cfg ,interp-pseudo-x86-2)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference" ,build-interference ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-pseudo-x86-2)
    ("patch instructions" ,patch-instructions ,interp-pseudo-x86-2)
    ("print x86" ,print-x86 #f)))
;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
         (lambda (p)
           (string=? r (car (string-split p "_"))))
         all-tests)))

(debug-level 1)
;(interp-tests "cond" type-check-Rif passes interp-Rvec "cond_test" (tests-for "cond"))
(interp-tests "functions" type-check-Rfun passes interp-Rfun "functions_test"
              (tests-for "functions"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;(compiler-tests "cond" type-check-Rif passes "cond_test" (tests-for "cond"))
;(compiler-tests "vectors" type-check-Rvec passes "vectors_test" (tests-for "vectors"))
