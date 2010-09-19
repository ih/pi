;(import (srfi :1 lists))
(import (church))
(church
 (define nats (list 'N '1 '(+ N N)))
 (define base (list nats))
 (define (get-rule-name rule)
   (first rule))

 (define (find test lst)
   (if (null? lst)
       false
       (let ((item (first lst)))
         (if (test item)
             item
             (find test (rest lst))))))



 (find (lambda (x) (= x 3)) '(1 2 3 4 5))

 
 (get-rule-name nats)
  
 (define (select-rule grammar rule-name)
   (find get-rule-name grammar))

 (select-rule base 'N)

 (define (choose-node rule)
   (uniform-draw (get-nodes rule)))
 (define (get-nodes rule)
   (rest rule))

 (repeat 5 (lambda () (choose-node (select-rule base 'N))))

 (define (has-children? node)
   (list? node))

 (define (get-children node)
   (rest node))
 (define (get-operator-name node)
   (first node))
 (define (expression-generator grammar)
   (define (generate-expression rule-name)
     (let* ((current-rule (select-rule grammar rule-name))
            (node (choose-node current-rule)))
       (if (has-children? node)
           (pair (get-operator-name node) (map generate-expression (get-children node)))
           node))))

 (define generate-expression-base (expression-generator base))
 (define texp (generate-expression-base 'N))
 (pretty-print texp)
 (define (gen-sym)
   (second (gensym)))
 (gen-sym)

 
 (define (list-possible-variables number)
  (repeat number gen-sym))
 (symbol? (first (list-possible-variables 4)))
 ;(symbol? (second (gensym)))



 (define (count-leaves expression)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (apply + (map count-leaves children)))
       1))

 (define num-vars (count-leaves texp))
 (define tvars (list-possible-variables num-vars))
 (pretty-print tvars)
 (define (insert-variable expression variables)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (pair (get-operator-name expression) (map insert-variable children)));; insert-variable takes two arguments need to curry it or something http://www.engr.uconn.edu/~jeffm/Papers/curry.html
       (if (flip)
           (uniform-draw variables)
           expression)))
; (insert-variable texp tvars)
 (symbol? (first tvars))
 (symbol? (uniform-draw tvars))
; (insert-variable '(+ 1 (+ 1 1)) '(a b c))
 (define (curry fun . args)
          (lambda x
            (apply fun (append args x))))
 ((curry * 2) 6)
 )
(exit)