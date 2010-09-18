;examples of expressions
(import (srfi :1 lists))
(import (enumeration))
;example expression 
(lambda (x y) (* (+ x y) 7))
;biggest value a number can take
(define range 10)
(define rest cdr)
(define (generate-data expr amount)
  (if (= amount 0)
      '()
      (cons (generate-example expr) (generate-data expr (- amount 1)))))

(define (generate-example expr)
  (let* ((inputs (random-values (variables expr)))
        (output (eval (cons expr inputs) (environment '(rnrs)))))
    (list inputs output)))

(define (random-values variables)
  (map random-value variables))
(define (random-value variable)
  (random range))

(define (variables expr) (second expr))

(define tl '(lambda (x y) (+ x y)))



;;;brute force approach by generating all parse trees and filtering good ones, then abstracting
(define e1 '((7 6) 72))
(define (good-trees example))


;see trainingData for a version that does not assume expressions come in the form (lambda variables ...)
;;;;;;;;;;;;;;;;;;;;;;;;;
