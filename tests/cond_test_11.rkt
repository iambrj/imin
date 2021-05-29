(let ([x (if (and #t (or #f #t)) (+ 1 2) (- 10 5))])
  (let ([x (+ x x)])
    (+ x 5)))
