#lang racket

(require "utilities.rkt"
         "utils.rkt")

(provide convert-to-closures)

(define (convert-to-closures p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info (map (lambda (d)
                              (struct-copy Def d
                                           [body (cc-exp (Def-body d))]))
                            def*))]))

; Closure-converted body X Top level definitions
(define (cc-exp e def*)
  (match e
    [(or (Void)
         (Int _)
         (Bool _)
         (Var _))
     (values e def*)]
    [(HasType e t)
     (let-values ([(e^ def*) (cc-exp e def*)]))
     (values (HasType (cc-exp e) t) def*)]
    [(Let v e b)
     (let-values ([(e^ e-def*) (cc-exp e def*)]
                  [(b^ b-def*) (cc-exp b def*)])
       (values (Let v e^ b^) (remove-duplicates (append e-def* b-def*))))]
    [(If c t e)
     (let-values ([(c^ c-def*) (cc-exp c def*)]
                  [(t^ t-def*) (cc-exp t def*)]
                  [(e^ e-def*) (cc-exp e def*)]))
     (values (If c^ t^ e^) (remove-duplicates (append c-def* t-def* e-def*)))]
    [(Prim op e*) #:when (member op '(read - + not < vector vector-length
                                           procedure-arity))
     (let ([e-def* (foldl (lambda (e e-def*)
                            (let-values ([(e^ e-def) (cc-exp e def*)])
                              `((,e^ . ,e-def) . ,e-def*)))
                          '()
                          e*)])
       (values (Prim op (map car e-def*)) (remove-duplicates (apply append (map cdr e-def*)))))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (let-values ([(e^ e-def*) (cc-exp e def*)])
       (values (Prim 'vector-ref `(,e^ ,(Int i))) e-def*))]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (let-values ([(e1^ e1-def*) (cc-exp e1 def*)]
                  [(e2^ e2-def*) (cc-exp e2 def*)]))
     (values (Prim 'vector-set! `(,e1^
                                   ,(Int i) ,e2^))
             (remove-duplicates (append e1-def* e2-def*)))]
    [(Apply fun arg*)
     ; TODO
     (Apply fun arg*)]
    [(Lambda param* rtype body)
     (let* ([param-name* (map param-name param*)]
            [fv* (set->list (free-vars (list->set param-name*) (set)))]
            [fname (gensym 'lambda)]
            [arity (length param*)]
            [body^ (foldr (lambda (i b)
                            (Let (list-ref fv* i)
                                 (Prim 'vector-ref (list (Var 'clos) (Int i)))
                                 b))
                          body
                          (reverse (range 0 (length fv*))))]
            [param-type ()]
            ; TODO param type, return type, info
            [def (Def fname () () ()
                      body^)])
       (values (Closure arity (cons (FunRef fname) fv*))
               (cons def def*)))]))

; Tail recursive, woohoo
(define (free-vars bound body fvs)
  (match e
    [(or (Void)
         (Int i)
         (Bool b))
     fvs]
    [(Var v)        (if (set-member? bound v) fvs (set->list (set-add fvs v)))]
    [(HasType e t)  (free-vars bound e fvs)]
    [(Let v e b)
     (let ([e-fvs (list->set (free-vars bound e fvs))]
           [b-fvs (list->set (free-vars (set-add bound v) b fvs))])
       (set->list (set-union e-fvs b-fvs)))]
    [(If c t e)
     (let ([c-fvs (list->set (free-vars bound c fvs))]
           [t-fvs (list->set (free-vars bound t fvs))]
           [e-fvs (list->set (free-vars bound e fvs))])
       (set->list (set-union c-fvs t-fvs e-fvs)))]
    [(Prim op e*) #:when (member op '(read - + not < vector vector-length
                                           procedure-arity))
     (let ([e-fvs* (map (lambda (e)
                          (list->set (free-vars bound e fvs)))
                        e*)])
       (set->list (apply set-union e-fvs*)))]
    [(Prim 'vector-ref `(,e ,(Int i)))
     (free-vars bound e fvs)]
    [(Prim 'vector-set! `(,e1 ,(Int i) ,e2))
     (let ([e1-fvs (list->set (free-vars bound e1 fvs))]
           [e2-fvs (list->set (free-vars bound e2 fvs))])
       (set->list (set-union e1-fvs e2-fvs)))]
    [(Apply fun arg*) fvs]
    [(Lambda param* rtype body)
     (let* ([param-name* (map param-name param*)]
            [param-names (list->set param-name*)])
       (free-vars (set-union param-names bound) body fvs))]))
