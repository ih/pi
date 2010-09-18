(import (church))

(church
 (define generate-tree
   (if (flip)
       'x
       (list '+ (generate-tree) (generate-tree))))
 
 (define (procedure-from-expression expr)
   (eval (list 'lambda '(x) expr) (get-current-environment)))


 (mh-query
  10 10

  (define my-expr (generate-tree))
  (define my-proc (procedure-from-expression my-expr))
  my-expr

  (and (= (my-proc 1) 4)
       ))

(exit)

