(if (and (eq? (read) 0) (eq? (read) 1))
  31
  40)
#|
(define p1 (Program
             '()
             (If
               (If
                 (Let 'tmp27281 (Prim 'read '()) (Prim 'eq? (list (Var 'tmp27281) (Int 0))))
                 (Let 'tmp27282 (Prim 'read '()) (Prim 'eq? (list (Var 'tmp27282) (Int 1))))
                 (Bool #f))
               (Int 0)
               (Int 42))))

=>

block25000:
  Return 0
block25001:
  Return 42
block25002:
  IfStmt (eq? var27282 1)
  Goto block25000
  Goto block25001
block25003:
  tmp27282 = (read)
  Goto block25002
block25004:
  IfStmt (eq? tmp27281 0)
  Goto block25003
  Goto block25001
start:
  tmp27281 = (read)
  Goto block25004

|#
