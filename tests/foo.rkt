#lang typed/racket

(define (other-car [x : (Vector Integer (Vector Integer))]
                   [y : (Vector (Vector Integer) Integer)]
                   [z : Integer]
                   [a : Boolean]
                   [b : (Vector Boolean Integer)]
                   [c : (Vector (Vector Integer) (Vector Boolean Boolean))]
                   [d : (Vector (Vector Integer) (Vector Integer))])
  : Integer
  (+ (vector-ref (vector-ref y 0) 0)
     (vector-ref (vector-ref d 0) 0)))
(other-car (vector 1 (vector 2))
           (vector (vector 3) 4)
           5
           #f
           (vector #f 6)
           (vector (vector 8) (vector #f #t))
           (vector (vector 10) (vector 12)))
