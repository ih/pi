(import (church))

(church
 (define baserate 0.1)
 (define samples
   (mh-query
    10 10
    (define generate-tree
      (if (flip)
          1
          (list '+ (generate-tree) (generate-tree))))
    (define answer (eval (generate-tree) (get-current-environment)))
    generate-tree
    (= 4 answer)
    ))
   (samples))
(exit)

