(if (and #t (or #f #t))
  (+ 1 2)
  (- 10 5))
#|
(If
  (If (Bool #t) (If (Bool #f) (Bool #f) (Bool #t)) (Bool #f))
  (Prim '+ (list (Int 1) (Int 2)))
  (Let
   'tmp27220
   (Prim '- (list (Int 5)))
   (Prim '+ (list (Int 10) (Var 'tmp27220)))))
(hash
  'block27221
  (Seq
   (Assign (Var 'tmp27220) (Prim '- (list (Int 5))))
   (Return (Prim '+ (list (Int 10) (Var 'tmp27220)))))
  'start
  (Goto 'block27221))
|#
