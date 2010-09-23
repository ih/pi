(import (church))

(church
;;;code for generating an parse-tree
 (define (generate-syntax-tree grammar rule-name)
   (let* ((current-rule (select-rule grammar rule-name))
          (operation (choose-operation current-rule)))
     (if (has-operands? operation)
         (construct-operation (get-operator operation) (map (curry generate-syntax-tree grammar) (get-operands operation)))
         operation)))

;;;code for generating a grammar
 (define (generate-grammar start-grammar)
   (let* ((number-of-rules (sample-integer max-number-rules))
          (rule-names (make-rule-names number-of-rules)))
     (generate-rules start-grammar rule-names)))

 (define (generate-rules grammar future-names)
   (if (null? future-names)
       grammar
       (let ((new-rule (generate-rule grammar (first future-names))))
         (generate-rules (add-rule new-rule grammar) (rest future-names)))))

 (define (generate-rule grammar rule-name)
   (let* ((rhs (generate-rhs grammar rule-name))
          (rule (construct-rule rule-name rhs))
          (old-names (get-rule-names grammar)))
     (ensure-non-circular rule old-names)))

 (define (generate-rhs grammar rule-name)
   (let* ((rhs-length (sample-positive-integer max-rhs-length)))
     (repeat rhs-length (curry generate-operator grammar rule-name))))

 (define (generate-operator grammar rule-name)
   (let* ((start-rule (uniform-draw grammar))
          (syntax-tree (generate-syntax-tree grammar (get-rule-name start-rule)))
          (procedure (generate-procedure syntax-tree))
          (number-of-args (count-args procedure))
          (possible-names (pair rule-name (get-rule-names grammar))) 
          (rule-names (repeat number-of-args (curry choose-rule-name possible-names))))
     (pair procedure rule-names)))

 (define (generate-procedure syntax-tree)
   (let* ((body (insert-variables syntax-tree))
          (head (create-head body)))
     (append head (list body))))

 (define (insert-variables syntax-tree)
  (let* ((upper-bound (count-leaves syntax-tree))
         (possible-variables (list-possible-variables upper-bound)))
    (insert-variables-recursion possible-variables syntax-tree)))

 (define (insert-variables-recursion variables syntax-tree)
   (if (operator? syntax-tree)
       (let ((operands (get-operands syntax-tree)))
         (construct-operation (get-operator syntax-tree) (map (curry insert-variables-recursion variables) operands)))
       (if (flip) 
           (uniform-draw variables)
           syntax-tree)))

;;;grammar implementation functions; a grammar is implemented as a list of rules
 (define (select-rule grammar rule-name)
   (find get-rule-name grammar))

 (define (get-rule-names grammar)
  (map first grammar))

;;;rule implementation functions; rules are implemented as a list with the first element being the name of the rule and the rest of the elements are operations

 (define construct-rule pair)
 
 (define get-rule-name first)

;rules should be modified to have a parameterized distribution over operations
 (define (choose-operation rule)
   (uniform-draw (get-operations rule)))

 (define get-operations rest)

;;;operation implementation functions; operations are implemented as a list with the first element being an operator (lambda syntax-tree) and the rest of the elements are the operands, within a rule the operands are represented by rule names 
 (define has-operands? list?)

 ;construct-operation takes two arguments, the operator and a list of operands
 (define construct-operation pair)

 (define get-operator first)
  
 (define get-operands rest)

;;;syntax tree implementation functions
 (define operator? list?)

 
 
;;;general functions
 (define (find test lst)
   (if (null? lst)
       false
       (let ((item (first lst)))
         (if (test item)
             item
             (find test (rest lst))))))

 (define (curry fun . args)
          (lambda x
            (apply fun (append args x))))
 ((curry * 2) 6)

;;;testing 
 (define naturals '((N 1 (+ N N))))
 (generate-syntax-tree naturals 'N)

 )

(exit)