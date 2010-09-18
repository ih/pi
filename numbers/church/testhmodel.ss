;(import (srfi :1 lists))
(import (church))
(church
 (define nats '(N 1 (+ N N)))
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
 (generate-expression-base 'N)  
 )
(exit)