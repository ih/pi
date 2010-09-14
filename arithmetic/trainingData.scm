;examples of expressions
(import (srfi :1 lists))

;example expression 
(* (+ 3 4) 7)
;biggest value a number can take
(define range 3)
(define rest cdr)
(define (generate-data expr amount)
  (if (= amount 0)
      '()
      (cons (generate-example expr) (generate-data expr (- amount 1)))))

(define (generate-example expr)
  (let* ((inputs (random-values (variables expr)))
        (output (evaluate expr inputs)))
    (pair inputs output)))


(define (random-values variables)
  (map random-value variables))
(define (random-value variable)
  (list variable (random range)))

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

