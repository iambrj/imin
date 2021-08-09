#lang racket

(require "utilities.rkt"
         "utils.rkt")

(provide remove-complex-opera*)

; returns atomic expression, new env
(define (rco-atm e)
  (match e
    [(Void)     (values (Void) '())]
    [(Int i)    (values (Int i) '())]
    [(Var v)    (values (Var v) '())]
    [(Bool b)   (values (Bool b) '())]
    [(HasType e t) #:when (atom? e)
     (values (HasType e t) '())]
    [(HasType e t)
     (let-values ([(atm env) (rco-atm e)])
       ; Invariant : atm is *always* a (gensym 'tmp), since other cases are
       ; caught by previous clauses
       (values (HasType (Var atm) t)
               (dict-set env atm (HasType (dict-ref env atm) t))))]
    [(Collect bytes)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(Collect bytes)))))]
    [(GlobalValue var)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(GlobalValue var)))))]
    [(Allocate bytes t)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(Allocate bytes t)))))]
    [(Apply fun arg*)
     (let* ([tmp (gensym 'tmp)]
            [fun-atm-env ((compose (lambda (atm env)
                                     (cons atm env))
                                   rco-atm) fun)]
            [fun-atm (car fun-atm-env)]
            [fun-atm (if (symbol? fun-atm)
                       (Var fun-atm)
                       fun-atm)]
            [fun-env (cdr fun-atm-env)]
            [arg-atm-env* (map (compose (lambda (atm env)
                                          (cons atm env))
                                        rco-atm)
                               arg*)]
            [arg-env* (filter-map (lambda (x)
                                    (and (not (empty? (cdr x)))
                                         (cdr x))) arg-atm-env*)]
            [arg-atm* (map (compose (lambda (atm)
                                      (if (symbol? atm)
                                        (Var atm)
                                        atm))
                                    car)
                           arg-atm-env*)])
       (values tmp (append (apply append (cons fun-env arg-env*))
                           `((,tmp . ,(Apply fun-atm arg-atm*))))))]
    [(FunRef f)
     (let ([tmp (gensym 'funref)])
       (values tmp `((,tmp . ,(FunRef f)))))]
    [(Prim op es)
     (let* ([tmp (gensym 'tmp)]
            [atm-envs (map (compose (lambda (atm env)
                                       (cons atm env))
                                     rco-atm)
                            es)]
            ; Invariant : alist ordered envs preserve order of execution
            [envs (filter-map (lambda (x)
                                (and (not (empty? (cdr x)))
                                     (cdr x))) atm-envs)]
            [atms (map (compose (lambda (atm)
                                  (if (symbol? atm)
                                    (Var atm)
                                    atm))
                                car)
                       atm-envs)])
       (values tmp (append (apply append envs)
                           `((,tmp . ,(Prim op atms))))))]
    [(If c t e)
     (let ([tmp (gensym 'tmp)])
       (values tmp `((,tmp . ,(If (rco-exp c) (rco-exp t) (rco-exp e))))))]
    [(Let x v b)
     (let ([v (rco-exp v)]
           [b (rco-exp b)])
       (if (atom? b)
         (values b `((,x . ,v)))
         (let ([tmp (gensym 'tmp)])
           (values tmp `((,tmp . ,(Let x (rco-exp v) (rco-exp b))))))))]))

(define (rco-exp e)
  (match e
    [(Void)             (Void)]
    [(Int i)            (Int i)]
    [(Var v)            (Var v)]
    [(Bool b)           (Bool b)]
    [(HasType e t)      (HasType (rco-exp e) t)]
    [(Allocate bytes t) (Allocate bytes t)]
    [(Collect bytes)    (Collect bytes)]
    [(GlobalValue var)  (GlobalValue var)]
    ; Invariant : f is always a symbol
    [(FunRef f)         (FunRef f)]
    [(Apply fun arg*)
     (let* ([tmp (gensym 'tmp)]
            [fun-atm-env ((compose (lambda (atm env)
                                     (cons atm env))
                                   rco-atm) fun)]
            [fun-atm (car fun-atm-env)]
            [fun-atm (if (symbol? fun-atm)
                       (Var fun-atm)
                       fun-atm)]
            [fun-env (cdr fun-atm-env)]
            [arg-atm-env* (map (compose (lambda (atm env)
                                          (cons atm env))
                                        rco-atm)
                               arg*)]
            [arg-env* (filter-map (lambda (x)
                                    (and (not (empty? (cdr x)))
                                         (cdr x))) arg-atm-env*)]
            [arg-atm* (map (compose (lambda (atm)
                                      (if (symbol? atm)
                                        (Var atm)
                                        atm))
                                    car)
                           arg-atm-env*)])
       (build-lets (apply append (cons fun-env arg-env*))
                   (Apply fun-atm arg-atm*)))]
    [(Prim op es)
     (let* ([atm-envs (map (compose (lambda (a e)
                                      (cons a e))
                                    rco-atm)
                           es)]
            [atms (map (compose (lambda (atm)
                                  (if (symbol? atm)
                                    (Var atm)
                                    atm))
                                car)
                       atm-envs)]
            ; Invariant : alist ordered envs preserve order of execution
            [envs (filter-map (lambda (x)
                                (and (not (empty? (cdr x)))
                                     (cdr x)))
                              atm-envs)])
       (build-lets (apply append envs) (Prim op atms)))]
    [(If c t e)
     (If (rco-exp c) (rco-exp t) (rco-exp e))]
    [(Let x v b) (Let x (rco-exp v) (rco-exp b))]))

; arguments of operations are atomic
(define (remove-complex-opera* p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info
                  (map (lambda (d)
                         (struct-copy Def d
                                      [body (rco-exp (Def-body d))]))
                       def*))]))
