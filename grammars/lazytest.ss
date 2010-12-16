(import (pi lazy))

(define l (lazy-list 'a 'b 'c))
(pretty-print (lazy-pair? l))