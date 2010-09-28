(import
 (church external py-pickle)
 (church church))

(define draw-trees
  (py-pickle-script "./treedraw.py"))

(draw-trees (cons "./test.png" (list '(a (b (c (c)) (b)) (d (e))) '(a (b) (c) (d)))))
;(draw-trees (format "~a.png" (hash (list '(a (b (c (c)) (b)) (d (e)))))) (list (a (b (c (c)) (b)) (d (e)))))

(exit)