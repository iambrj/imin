(define (othercar [x : (Vector Integer (Vector Integer))]
                  [y : (Vector (Vector Integer) Integer)])
  : Integer
  (vector-ref (vector-ref y 0) 0))
(othercar (vector 1 (vector 2))
          (vector (vector 3) 4))
