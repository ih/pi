;examples of expressions
(import (srfi :1 lists))

;example expression 
(lambda (x y) (* (+ x y) 7))
;biggest value a number can take
(define range 3)
(define rest cdr)
(define (generate-data expr amount)
  (if (= amount 0)
      '()
      (cons (generate-example expr) (generate-data expr (- amount 1)))))

(define (generate-example expr)
  (let* ((inputs (random-values (variables expr)))
        (output (eval (list apply expr inputs) (environment '(rnrs)))))
    (list inputs output)))

(define (random-values variables)
  (map random-value variables))
(define (random-value variable)
  (random range))

(define (variables expr) (second expr))

(define tl '(lambda (x y) (+ x y)))

;see trainingData for a version that does not assume expressions come in the form (lambda variables ...)
;;;;;;;;;;;;;;;;;;;;;;;;;


(define (variables expr)
  (if (variable? expr)
      expr
      (if (subexpressions expr)
          (delete-duplicates (flatten (map variables (subexpressions expr))))
          '())))

(define (flatten l)
  (cond ((null? l) '())
        ((list? l)
         (append (flatten (car l)) (flatten (cdr l))))
        (else (list l))))


(define (variable? expr)
  (and (symbol? expr) (not (operator? expr))))
(define (operator? expr)
  (member expr '(+ - / *)))

;assumes expression is either a primitive or operator with arguments that are expressions
(define (subexpressions expr)
  (if (primitive? expr)
      '()
      (rest expr)))

(define (primitive? expr)
  (or (null? expr) (number? expr) (variable? expr)))

