#lang racket

(require graph
         "utilities.rkt"
         "priority_queue.rkt"
         "constants.rkt")

(provide allocate-registers)

(define (allocate-registers d)
  (match d
    [(ProgramDefs info def*)
     (ProgramDefs info (map ar-def def*))]))

(define (ar-def d)
  (match d
    [(Def name param* rty info label-block*)
     (let*-values ([(ig) (dict-ref info 'conflicts)]
                   [(vertex->color) (color-graph ig)]
                   [(var->mem stack-space shadow-stack-space)
                    (var->mem vertex->color)]
                   [(label-block*) (substitute var->mem label-block*)]
                   [(used-callee)
                    (filter-used-callee var->mem)])
       (Def name param* rty (dict-set* info
                                       'stack-space stack-space
                                       'shadow-stack-space shadow-stack-space
                                       'var->mem var->mem
                                       'used-callee used-callee)
            label-block*))]))

(define (color-graph ig)
  (let* ([vars (filter Var? (get-vertices ig))]
         [regs (filter Reg? (get-vertices ig))]
         [vertex->color (init-vertex->color regs)]
         [vertex->sat (init-vertex->sat vars ig vertex->color)]
         [q (init-pq vertex->sat)]
         [vertex->node (map (lambda (v) (cons v (pqueue-push! q v))) vars)])
    (color-graph-iter ig q vars vertex->sat vertex->node
                      vertex->color)))

(define (init-vertex->color regs)
  (foldr (lambda (reg a)
           (if (member reg usable-registers)
             (dict-set a reg (index-of
                               usable-registers
                               reg))
             a))
         '()
         regs))

(define (init-vertex->sat vars ig vertex->color)
  (make-hash (foldr (lambda (v a)
                      (let* ([n (get-neighbors ig v)]
                             [s (foldr (lambda (n a)
                                         (if (dict-has-key? vertex->color n)
                                           (set-union a (set (dict-ref vertex->color n)))
                                           a))
                                       (set)
                                       n)])
                        `((,v . ,s) . ,a))) '() vars)))

(define (init-pq vertex->sat)
  (make-pqueue (lambda (x1 x2)
                 (> (set-count (dict-ref vertex->sat x1))
                    (set-count (dict-ref vertex->sat x2))))))

(define (color-graph-iter g q w vertex->satur vertex->node vertex->color)
  (match w
    ['() vertex->color]
    [else (let* ([v (pqueue-pop! q)]
                 [w (set-remove w v)]
                 [satur (dict-ref vertex->satur v)]
                 [color (min-free satur)]
                 [neighbors (filter Var? (get-neighbors g v))]
                 [_ (foldr (lambda (x a)
                             (pqueue-decrease-key! q (dict-ref vertex->node x)))
                           '()
                           neighbors)]
                 [vertex->satur (update-neighbors! vertex->satur color neighbors)])
            (color-graph-iter g q w vertex->satur vertex->node (dict-set
                                                                 vertex->color v
                                                                 color)))]))

(define (update-neighbors! vertex->satur color neighbors)
  (match neighbors
    [`() vertex->satur]
    [`(,n . ,d)
      (let* ([s (dict-ref vertex->satur n)]
             [_ (dict-set! vertex->satur n (set-union s (set color)))])
        (update-neighbors! vertex->satur color d))]))

(define (var->mem var->color)
  (let* ([color->reg (map (lambda (r)
                            (cons (index-of usable-registers r) r))
                          usable-registers)]
         [maxcolor (apply max (dict-keys color->reg))]
         [spills (spill maxcolor var->color)]
         [spilled->stk (spill->stk spills)])
    (values (dict-map var->color (lambda (v c)
                                   `(,v . ,(if (member v spills)
                                             (dict-ref spilled->stk v)
                                             (dict-ref color->reg c)))))
            (stack-size Var? spills)
            (stack-size Vector? spills))))

(define (spill maxcolor var->color)
  (filter (lambda (v)
            (> (dict-ref var->color v) maxcolor))
          (dict-keys var->color)))

(define (spill->stk spills)
  (let ([vec-spills (filter Vector? spills)]
        [nonvec-spills (filter Var? spills)])
    (foldr (lambda (v i a)
           (let ([stk (if (Var? v) stk-reg shadow-stk-reg)]
                 [offset (if (Var? v)
                           (index-of nonvec-spills v)
                           (index-of vec-spills v))])
            `((,v . ,(Deref stk (* 8 offset))) . ,a)))
         '()
         spills
         (range 1 (add1 (length spills))))))

(define (stack-size pred spills)
  (* 8 (length (filter pred spills))))

; NOTE : Can be optimized to O(n) using some obscure mutation tricks
(define (min-free s)
  (let* ([l (set->list s)]
         [l (sort l <)])
    (if (or (equal? '() l) (> (car l) 0))
      0
      (min-free-list l))))

(define (min-free-list l)
  (match l
    [`(,a) (add1 a)]
    [`(,x . (,y . ,d))
      (if (= (add1 x) y)
        (min-free-list `(,y . ,d))
        (add1 x))]))

(define (substitute var->mem label-block*)
  (map (lambda (label-block)
         (cons (car label-block)
               ((substitute-block var->mem) (cdr label-block))))
       label-block*))

(define ((substitute-block var->mem) blk)
  (match blk
    [(Block info instrs)
     (Block info (map (substitute-instr var->mem) instrs))]))

(define ((substitute-instr var->mem) instr)
  (match instr
    [(Instr i args) (Instr i (map (substitute-arg var->mem) args))]
    [(TailJmp v c) (TailJmp ((substitute-arg var->mem) v) c)]
    [_ instr]))

(define ((substitute-arg var->mem) arg)
  (match arg
    [(Var v) (dict-ref var->mem (Var v))]
    [_ arg]))

(define (filter-used-callee var->mem)
  (let* ([vars (filter Var? (dict-keys var->mem))]
         [mems (for/list ([var vars]) (dict-ref var->mem var))])
    (filter (lambda (x) (member x callee-saved)) mems)))
