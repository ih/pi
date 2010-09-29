(import
 (church external py-pickle)
 (church church))

(define draw-trees
  (py-pickle-script "./treedraw.py"))

(draw-trees (cons "./test.png" (list '(a (b (c (c)) (b)) (d (e))) '(a (b) (c) (d)) '(c (d (e (b (c (a)))))))))


(exit)
