(let ([x (read)])
  (let ([y (if (< x 10) (if (< x 5) (+ x 2) (+ x 3)) (if (< x 8) (+ x 1) x))])
    (+ y 3)))
