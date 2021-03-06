(import (church))

(church
;;;global parameters
 (define max-number-rules 3)
 (define max-rhs-length 5)
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
          (number-of-variables (count-variables procedure))
          (possible-names (pair rule-name (get-rule-names grammar))) 
          (rule-names (repeat number-of-variables (curry choose-rule-name possible-names))))
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

 (define (list-possible-variables number)
   (repeat number gen-sym))

 (define (make-rule-names number)
   (repeat number gen-sym))

 (define choose-rule-name uniform-draw)
 
 (define (ensure-non-circular rule old-names)
   (if (circular? rule)
       (let ((new-rule (make-non-cicular rule old-names)))
         new-rule)
       rule))
;;;procedure implementation functions
 (define (count-variables procedure)
   (let ((variables (second procedure)))
     (length variables)))

 (define (create-head body)
   (let ((variable-names (get-variables body)))
     (list 'lambda variable-names)))

 (define (get-variables body)
   (delete-duplicates (flatten (get-variables-recursion body))))
 (define (get-variables-recursion body)
   (if (operator? body)
       (let ((operands (get-operands body)))
         (append (map get-variables-recursion operands)))
       (if (variable? body)
           body
           '())))

 (define variable? symbol?)
;;;grammar implementation functions; a grammar is implemented as a list of rules
 (define (select-rule grammar rule-name)
   (find (lambda (rule) (equal? (get-rule-name rule) rule-name)) grammar))

 (define (get-rule-names grammar)
  (map first grammar))

 (define add-rule pair)
;;;rule implementation functions; rules are implemented as a list with the first element being the name of the rule and the rest of the elements are operations

 (define construct-rule pair)
 
 (define get-rule-name first)

;rules should be modified to have a parameterized distribution over operations
 (define (choose-operation rule)
   (uniform-draw (get-operations rule)))

 (define get-operations rest)

 (define (circular? rule)
   (let* ((rule-name (get-rule-name rule))
          (rhs (get-rule-rhs rule)))
     (apply and (map (curry self-reference? rule-name) rhs))))

 (define (self-reference? rule-name node)
   (if (operator? node)
       (let ((operands (get-operands node)))
         (member? rule-name operands))
       (equal? node rule-name)))


 (define (make-non-cicular rule old-names)
   (construct-rule (get-rule-name rule) (pair (uniform-draw old-names) (get-rule-rhs rule))))

 (define get-rule-rhs rest)
 
;;;operation implementation functions; operations are implemented as a list with the first element being an operator (lambda syntax-tree) and the rest of the elements are the operands, within a rule the operands are represented by rule names 
 (define has-operands? list?)

 ;construct-operation takes two arguments, the operator and a list of operands
 (define construct-operation pair)

 (define get-operator first)
  
 (define get-operands rest)

;;;syntax tree implementation functions
 (define operator? list?)

 (define (count-leaves syntax-tree)
   (if (operator? syntax-tree)
       (let ((operands (get-operands syntax-tree)))
         (apply + (map count-leaves operands)))
       1))

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

 (define (sample-positive-integer n)
   (+ (sample-integer n) 1))

 (define (gen-sym)
   (second (gensym)))
 
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

 (define (flatten l)
   (cond ((null? l) '())
         ((list? l)
          (append (flatten (first l)) (flatten (rest l))))
         (else (list l))))

 (define (member? item lst)
  (if (null? lst)
      false
      (if (equal? item (first lst))
          true
          (member? item (rest lst)))))

 
;;;testing 
 (define naturals '((N 1 (+ N N))))
 ;(generate-syntax-tree naturals 'N)
 (generate-grammar naturals)

 (define dg '((g0 ((lambda () ((lambda () 1)))) ((lambda () ((lambda (g1) g1) ((lambda () 1))))) ((lambda () ((lambda (g2) g2) ((lambda () 1))))) ((lambda () ((lambda () 1))))) (g3 ((lambda (g2) g2) N) ((lambda () 1)) ((lambda (g1) g1) g3) ((lambda (g4) g4) g3)) (N 1 (+ N N))))
 (generate-syntax-tree dg 'g3)


 )

(exit)