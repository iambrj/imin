(define (car [x : (Vector Integer (Vector Integer))]) : Integer 
  (vector-ref x 0))                                             
(car (vector 1 (vector 2)))                                     
