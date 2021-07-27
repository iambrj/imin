(define (car [x : (Vector Integer)]) : Integer 
  (vector-ref x 0))                                             

(car (vector 1))                                     
