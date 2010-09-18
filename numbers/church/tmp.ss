(import (church))

(church
 
 (define (generate-tree)
   (if (flip)
       1
       (list '+ (generate-tree) (generate-tree))))
 (define (wrap)
   (list 'lambda '(x) (generate-tree)))
 (define answer (wrap))
 (pretty-print answer)
 (eval (list answer 3) (get-current-environment))
 (eval '(define a (lambda () (+ 2 2))) (get-current-environment))
 (eval '(a) (get-current-environment)))   
(exit)

