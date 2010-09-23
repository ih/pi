(import (srfi :1 lists))
(import (church))
(church

 (define max-number-rules 2)
 (define max-rhs-length 2)
 (define naturals '((N 1 (+ N N))))
 


 
 (define (generate-grammar start-grammar)
   (let* ((number-of-rules (sample-integer max-number-rules))
          (rule-names (make-rule-names number-of-rules)))
     (generate-rules start-grammar rule-names)))
 (define (make-rule-names number)
   (repeat number gen-sym))

 (define (generate-rules grammar future-names)
   (if (null? future-names)
       grammar
       (let ((new-rule (generate-rule grammar (first future-names))))
         (generate-rules (add-rule new-rule grammar) (rest future-names)))))

 (define add-rule pair)
                                        ;look into removing duplicate rules
 (define (generate-rule grammar name)
   (let* ((rhs (generate-rhs grammar name))
          (rule (pair name rhs))
          (old-names (get-rule-names grammar)))
     (ensure-non-circular rule old-names)))

 (define (generate-rhs grammar name)
   (let* ((rhs-length (sample-positive-integer max-rhs-length)))
     (repeat rhs-length (curry generate-operator grammar name))))

 (define (sample-positive-integer n)
   (+ (sample-integer n) 1))

                                        ;add a node that goes to the previous level if it is circular
 (define (ensure-non-circular rule old-names)
   (if (circular? rule)
       (let ((new-rule (make-non-cicular rule old-names)))
         new-rule)
       rule))
 (define (make-non-cicular rule old-names)
   (pair (get-rule-name rule) (pair (uniform-draw old-names) (get-rule-rhs rule))))

 (define (circular? rule)
   (let* ((rule-name (get-rule-name rule))
          (rhs (get-rule-rhs rule)))
     (apply and (map (curry self-reference? rule-name) rhs))))

 (define (self-reference? rule-name node)
   (if (operator? node)
       (let ((operands (get-operands node)))
         (member? rule-name operands))
       (equal? node rule-name)))

 (define get-operands rest)
 (define operator? list?)
 (define get-rule-rhs rest)

 (define (generate-operator grammar rule-name)
   (let* ((start-rule (uniform-draw grammar))
          (expression (generate-expression grammar (get-rule-name start-rule)))
          (procedure (define-procedure expression))
          (number-of-args (count-args procedure))
          (possible-names (pair rule-name (get-rule-names grammar))) 
          (rule-names (repeat number-of-args (curry choose-rule-name possible-names))))
     (pair procedure rule-names)))

 (define (get-rule-names grammar)
   (map first grammar))

 (define choose-rule-name uniform-draw)
 
;;;begin expression generator ****************
 
 (define (generate-expression grammar rule-name)
   (let* ((current-rule (select-rule grammar rule-name))
          (node (choose-node current-rule)))
     (if (has-children? node)
         (pair (get-operator-name node) (map (curry generate-expression grammar) (get-children node)))
         node)))

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


 (define get-rule-name first)

 (define (choose-node rule)
   (uniform-draw (get-nodes rule)))
 (define (get-nodes rule)
   (rest rule))

 (define has-children? list?)

 
 
 (define (get-children node)
   (rest node))

 (define (get-operator-name node)
   (first node))

;;;end expression-generator *************************

;;;begin define procedure; in this version we assume the grammar has no variables,this makes eval easy,  and we only replace leaves with variables

 (define (define-procedure expression)
   (let* ((body (insert-variables expression))
          (head (create-head body)))
     (append head (list body))))


 (define (insert-variables expression)
   (let* ((upper-bound (count-leaves expression))
          (possible-variables (list-possible-variables upper-bound)))
     (insert-variables-recursion possible-variables expression)))

 (define (insert-variables-recursion variables expression)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (pair (get-operator-name expression) (map (curry insert-variables-recursion variables) children)))
       (if (flip)
           (uniform-draw variables)
           expression)))

 
 (define (count-leaves expression)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (apply + (map count-leaves children)))
       1))

 (define (list-possible-variables number)
   (repeat number gen-sym))

 (define (gen-sym)
   (second (gensym)))

 
 (define (insert-variable-recursion variables expression)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (pair (get-operator-name expression) (map (curry insert-variable-recursion variables) children)))
       (if (flip)
           (uniform-draw variables)
           expression)))
 
 (define (create-head body)
   (let ((variable-names (get-variables body)))
     (list 'lambda variable-names)))

 (define (flatten l)
   (cond ((null? l) '())
         ((list? l)
          (append (flatten (first l)) (flatten (rest l))))
         (else (list l))))

 (define (get-variables body)
   (flatten get-variables-recursion body))
 (define (get-variables-recursion body)
   (if (has-children? body)
       (let ((children (get-children body)))
         (append (map get-variables-recursion children)))
       (if (variable? body)
           body
           '())))
 (define variable? symbol?)

 (define (delete-duplicates lst)
   (delete-duplicates-helper '() lst))

 (define (delete-duplicates-helper set lst)
   (if (null? lst)
       set
       (delete-duplicates-helper
        (if (member? (first lst) set)
            set
            (pair (first lst) set))
        (rest lst))))

 (define (member? item lst)
   (if (null? lst)
       false
       (if (equal? item (first lst))
           true
           (member? item (rest lst)))))
;;;end of define procedure

 (define (count-args procedure)
   (let ((variables (second procedure)))
     (length variables)))

 (define (get-body procedure)
   (third procedure))


;;;begin define-and-add-to-environment
 (define (define-and-add-to-environment procedure)
   (let ((name (get-procedure-name procedure)))
     (begin
       (eval procedure (get-current-environment))
       name)))

 (define (get-procedure-name procedure)
   (first (second procedure)))

 (generate-grammar naturals)
 )
(exit)