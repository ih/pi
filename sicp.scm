;(import (church))
(import (srfi :1 lists))
;eval for primitives
(define (eval expr)
  ;check whether expr is a primitive
  (cond ((primitive? expr) expr)
        ((composition? expr)
         (cond ((arithmetic? expr) (evalArithmetic expr))))
        ((abstraction? expr) expr)))

(define (primitive? expr)
  (or (number? expr)))


(define (composition? expr)
  (arithmetic? expr))

(define (arithmetic? expr)
  (cond ((not (list? expr)) (number? expr))
        (else (and (arithmeticOperator? (first expr)) (arithmetic? (second expr)) (arithmetic? (third expr))))))

(define (evalArithmetic expr)
  (if (not (list? expr))
      expr
      (let ((operator (first expr)))
        (cond ((equal? operator '+) (+ (evalArithmetic (second expr)) (evalArithmetic (third expr))))))))

(define (arithmeticOperator? symbol)
  (member symbol '(+ - / *)))

(define (abstraction? expr) #t)





(define (deval result)
  (sampleOperators )

;for primitives deval is the same as eval

;; (church
;;  (define-record-type primitive)
;;  (mh-query
;;   100 100

;;   (define input 5)
;;   (define (eval expr)
;;     (if (number? expr)
;;         expr))
;;   input 
;;   (eval input)
  
;;   )