(if (if (if (and #t (or #f #t)) #f #t) (eq? 4 5) (>= 8 8))
  (+ 5 3)
  (+ 6 7))
#|
(if (if (if (and #t (or #f #t)) #f #t) (eq? 4 5) (< 8 8))
  (+ 5 3)
  (+ 6 7))
|#
