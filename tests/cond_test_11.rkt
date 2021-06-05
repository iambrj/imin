(let ([x (if (and #t (or #f #t)) (+ 1 2) (- 10 5))])
  (let ([x (+ x x)])
    (+ x 5)))

#|
(Let
  'x27230 ; 3
  (If
   (If (Bool #t) (If (Bool #f) (Bool #f) (Bool #t)) (Bool #f))
   (Prim '+ (list (Int 1) (Int 2)))
   (Let
    'tmp27232
    (Prim '- (list (Int 5)))
    (Prim '+ (list (Int 10) (Var 'tmp27232)))))
  (Let
   'x27231
   (Prim '+ (list (Var 'x27230) (Var 'x27230)))
   (Prim '+ (list (Var 'x27231) (Int 5)))))

(hash
  'block27233
  (Seq
    ; -5
   (Assign (Var 'tmp27232) (Prim '- (list (Int 5))))
   (Seq
     ; 5
    (Assign (Var 'x27230) (Prim '+ (list (Int 10) (Var 'tmp27232))))
    (Seq
      ; 10
     (Assign (Var 'x27231) (Prim '+ (list (Var 'x27230) (Var 'x27230))))
     ; 15
     (Return (Prim '+ (list (Var 'x27231) (Int 5)))))))
  'start
  (Goto 'block27233))
|#
