(import (srfi :1 lists))
(import (church))
(church
 (define generate-grammar
   '())
   ;(mem (lambda '())))
 ;(define (generate-parse grammar '())
 (define (test x y)
   (+ x y))
 (define generate-operator (grammar start)
   (let* ((expression (random-expression grammar start))
          (procedure (define-procedure expression))
          (number-of-args (count-args procedure))
          (procdeure-name (define-and-add-to-environment procedure))
          (rule-names (repeat number-of-args random-existing-rule-name)))
     (pair procedure-name rule-names)))

 (define (expression-generator grammar)
   (define (generate-expression rule-name)
     (let* ((current-rule (select-rule grammar rule-name))
            (node (choose-node current-rule)))
       (if (has-children? node)
           (pair (get-operator-name node) (map generate-expression (get-children node)))
           node))))

 
 (define (select-rule grammar rule-name)
   (find get-rule-name grammar))

 (define (find test lst)
   (if (null? lst)
       false
       (let ((item (first lst)))
         (if (test item)
             item
             (find test (rest lst))))))


 (define (get-rule-name rule)
   (first rule))

 (define (choose-node rule)
   (uniform-draw (get-nodes rule)))
 (define (get-nodes rule)
   (rest rule))

 (define (has-children node)
   (list? node))
       
 
 (define (get-children node)
   (rest node))

 (define (get-operator-name node)
   (first node))

)

