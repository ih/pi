(import (church))
(church
 (define (scope-test func b)
   (func 3))

 (define (f x)
   (+ x b))

 (scope-test f 4)
 )
   