#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Rvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "compiler.rkt")
;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; should be named "compiler.rkt"
(define passes
  `( ("uniquify" ,uniquify ,interp-Rvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Rvar)
    ("explicate control" ,explicate-control ,interp-Cvar)
    ("instruction selection" ,select-instructions ,interp-x86-0)
    ("uncover live" ,uncover-live ,interp-x86-0)
    ("build interference" ,build-interference ,interp-x86-0)
    ("allocate registers" ,allocate-registers ,interp-x86-0)
    ; ("assign homes" ,assign-homes ,interp-x86-0)
    ("patch instructions" ,patch-instructions ,interp-x86-0)
    ; ("print x86" ,print-x86 #f)
    ;; Uncomment the following passes as you finish them.
    ))

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
(interp-tests "var" #f passes interp-Rvar "var_test" (tests-for "var"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;; (compiler-tests "var" #f passes "var_test" (tests-for "var"))
