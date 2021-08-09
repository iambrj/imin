#lang racket

(require graph
         "utilities.rkt")

(provide build-cfg)

(define (build-cfg p)
  (match p
    [(ProgramDefs info def*)
     (ProgramDefs info (map bc-def def*))]))

(define (bc-def d)
  (match d
    [(Def name param* rty info label-block*)
     (let* ([labels (dict-keys label-block*)]
            [g (directed-graph '())]
            [_ (for ([v labels]) (add-vertex! g v))]
            [_ (map (add-edges! g) label-block*)])
       (printf "CFG : ~a\n" (graphviz g))
       (Def name param* rty (dict-set info 'cfg g) label-block*))]))

(define ((add-edges! g) label-block)
  (match (cdr label-block)
    [(Block info instrs)
     (map (add-edges-instr! g (car label-block)) instrs)]
    [else (error "add-edges! unhandled case " label-block)]))

(define ((add-edges-instr! g c) instr)
  (match instr
    [(Jmp l)
     (add-directed-edge! g c l)]
    [(JmpIf _ l)
     (add-directed-edge! g c l)]
    [else (void)]))
