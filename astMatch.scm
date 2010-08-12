(define e1 '(- (+ 3 4) 6))
(define e2 '(- 7 6))
(define e3 '(- (* 3 7) 6))

;take a list of expressions and return a library that can reproduce the expressions
(define (astMatch exprLst)
  (matchLeaves (map leaves exprLst)))