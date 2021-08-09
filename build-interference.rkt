#lang racket

(require graph
         "utilities.rkt"
         "utils.rkt"
         "constants.rkt")

(provide build-interference)

(define (build-interference d)
  (match d
    [(ProgramDefs info def*)
     (ProgramDefs info (map bi-def def*))]))

(define (bi-def d)
  (match d
    [(Def name param* rty info label-block*)
     (let* ([g (undirected-graph '())]
            [types (dict-ref info 'locals-types)]
            [_ (map (compose (bi-block g types) cdr) label-block*)])
       (printf "Interference graph: ~a\n" (graphviz g))
       (Def name param* rty (dict-set info 'conflicts g) label-block*))]))

(define ((bi-block g types) blk)
  (match blk
    [(Block info instrs)
     (let* ([live-after (dict-ref info 'live-after)]
            [vertices (apply set-union live-after)]
            [_ (for ([v (set->list vertices)]) (add-vertex! g v))]
            [_ (for ([i instrs] [la live-after]) (bi-instr i la g types))])
       g)]))

(define (bi-instr instr live-after g types)
  (match instr
    [(Instr 'movq `(,s ,d))
     (let* ([d (deref->reg d)]
            [_ (for ([v (map deref->reg (set->list live-after))])
                 (unless (or (equal? v d) (equal? v s)) (add-edge! g d v)))])
       g)]
    ; Don't want TailJmp's argument to be a callee-saved register, since we pop
    ; off callee saved registers before tail jumping!
    [(Instr 'leaq `(,_ ,d))
     (let* ([_ (for* ([d (map deref->reg (set->list (instr-w instr)))]
                      [v (map deref->reg (set->list live-after))])
                 (unless (equal? v d) (add-edge! g d v)))]
            [_ (for* ([r callee-saved])
                 (add-edge! g d r))])
       g)]
    [(Callq 'collect _)
     (let* ([v-live (filter (lambda (x)
                              (and (symbol? x)
                                   (Vector? (dict-ref types x))))
                            (set->list live-after))]
            [_ (for* ([v (map deref->reg v-live)]
                      [r (map deref->reg callee-saved)])
                 (add-edge! g v r))])
       g)]
    [else (let* ([_ (for* ([d (map deref->reg (set->list (instr-w instr)))]
                           [v (map deref->reg (set->list live-after))])
                      (unless (equal? v d) (add-edge! g d v)))])
            g)]))

(define (deref->reg d)
    (match d
      [(Deref r _) (Reg r)]
      [_ d]))
