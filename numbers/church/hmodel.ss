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
   (let* ((generate-expression (expression-generator grammar))
          (expression (generate-expression start))
          (procedure (define-procedure expression))
          (procdeure-name (define-and-add-to-environment procedure))
          (rule-names (repeat number-of-args random-existing-rule-name)))
     (pair procedure-name rule-names)))
;;;begin expression generator ****************
 (define (expression-generator grammar)
   (define (generate-expression rule-name)
     (let* ((current-rule (select-rule grammar rule-name))
            (node (choose-node current-rule)))
       (if (has-children? node)
           (pair (get-operator-name node) (map generate-expression (get-children node)))
           node))))
 (define (curry fun . args)
          (lambda x
            (apply fun (append args x))))

 
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

;;;end expression-generator *************************

;;;begin define procedure; in this version we assume the grammar has no variables,this makes eval easy,  and we only replace leaves with variables
(define (define-procedure expression)
  (let* ((body (insert-variables expression))
         (head (create-head body)))
    (pair head body)))

(define (insert-variables variables expression)
  (let* ((upper-bound (count-leaves expression))
         (possible-variables (list-possible-variables upper-bound))
         (attempts (sample-integer upper-bound)))
    (repeat attempts (insert-variable expression possible-variables))))

(define (count-leaves expression)
  (if (has-children? expression)
      (let ((children (get-children expression)))
        (apply + (map count-leaves children)))
      1))

(define (list-possible-variables number)
  (repeat number gen-sym))

(define (gen-sym)
  (second (gensym)))

         
(define (insert-variable expression variables)
  (if (has-children? expression)
      (let ((children (get-children expression)))
        (pair (get-operator-name expression) (map insert-variable children)))
      (if (flip)
          (uniform-draw variables)
          expression)))
          
 
 
)

