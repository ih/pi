(import (church))
(church
 (define (random-arithmetic-expression)
   (if (flip)
       1
       (list '+ (random-arithmetic-expression) (random-arithmetic-expression))))

 (define (procedure-from-expression expr)
   (eval (list 'lambda '(x) expr) (get-current-environment)))

 (mh-query
  100 100

  (define my-expr (random-arithmetic-expression))
  (define my-proc (procedure-from-expression my-expr))

  my-expr

  (and (= (my-proc 1) 4)) 
  )
 )