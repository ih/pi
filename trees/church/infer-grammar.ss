(import (church))

(church
 (debug-mode 'trace-eval true)
;;;global parameters
 (define max-number-rules 3)
 (define max-rhs-length 5)
;;;generative model
    (define debug 2); if this is -1 then generate-grammar should work normally otherwise each call to generate-syntax-tree will be limited to debug number of iterations
;;;code for generating a syntax-tree
    (define (generate-partial-syntax-tree time grammar rule-name)
      (let* ((current-rule (select-rule grammar rule-name))
             (operation (choose-operation current-rule))
             )
        (if (and (has-operands? operation) (not (times-up? time)))
            (construct-operation (get-operator operation) (map (curry generate-partial-syntax-tree (adjust-time time) grammar) (get-operands operation)))
            operation)))

    (define (generate-syntax-tree time grammar rule-name)
      (make-evalable grammar (generate-partial-syntax-tree time grammar rule-name)))

;;;code for generating a grammar
    (define (generate-grammar start-grammar)
      (let* ((number-of-rules (sample-integer max-number-rules))
             (rule-names (make-rule-names number-of-rules)))
        (generate-rules start-grammar rule-names)))
    ;; (define generate-grammar
    ;;   (mem
    ;;    (lambda (start-grammar)
    ;;      (let* ((number-of-rules (sample-integer max-number-rules))
    ;;             (rule-names (make-rule-names number-of-rules)))
    ;;        (generate-rules start-grammar rule-names)))))

    
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
             (syntax-tree (generate-syntax-tree debug grammar (get-rule-name start-rule)))
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
    (define (has-operands? op)
      (and (list? op) (not (null? op))))

                                        ;construct-operation takes two arguments, the operator and a list of operands
    (define construct-operation pair)

    (define get-operator first)
    
    (define get-operands rest)

;;;syntax tree implementation functions
    (define (operator? op)
      (and (list? op) (not (null? op))))

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

;;;bounded generation related functions
    (define (times-up? time) (= 0 time))
    (define (adjust-time time) (- time 1))
                                        ;for now make-evalable turns rule names into 1s, this can be generalized to replacing the rule names with primitives from the type represented by the rule name
    (define (make-evalable grammar partial-syntax-tree)
      (let ((rule-names (get-rule-names grammar)))
        (make-evalable-recursion rule-names partial-syntax-tree)))

    (define (make-evalable-recursion rule-names partial-syntax-tree)
      (if (operator? partial-syntax-tree)
          (let* ((operands (get-operands partial-syntax-tree)))
            (construct-operation (get-operator partial-syntax-tree) (map (curry make-evalable-recursion rule-names) operands)))
          (if (member? partial-syntax-tree rule-names)
              '() ;the general case will depend on rule name
              partial-syntax-tree)))
    
;;;
;;;tree grammar related functions
    (define trees '((T ((lambda (x) (append '(node color) (flatten-empty x))) A)) (A ((lambda (x) (list x)) T) () ((lambda (x y) (append (flatten-empty x) (flatten-empty y))) A A))))

    (define (flatten-empty lst)
      (if (all-empty? lst)
          '()
          lst))
    (define (all-empty? lst)
      (if (list? lst)
          (if (null? lst)
              true
              (apply and (map all-empty? lst)))
          false))
              
      
    (define (join x y)
      (cond ((null? x) y)
            ((null? y) x)
            (else (list 'join x y))))
    (define (flatten-join jlist)
      (if (jlist? jlist)
          (let ((items (get-jlist-items jlist)))
            (append (list (flatten-join (first items))) (flatten-join (rest items))))
          (list jlist)))


    
(define (jlist? lst)
  (if (list? lst)
      (equal? (first lst) 'join)
      false))

(define get-jlist-items rest)

;;;inference
;;  (define samples
;;    (mh-query
;;     100 100naturals
;;     (define trees '((T ((lambda (x) (append '(node color) x)) A)) (A T '() (flatten (join A A)))))
;;     (naturals ((N 1 (+ N N)))
;;     (define grammar (generate-grammar naturals))
;;     (define start-rule (get-rule-name (first grammar)))
;;     ;(define expr (generate-syntax-tree 5 naturals 'N))

;; ;;;what we want to know
;;     (list "grammar" grammar "expression"(generate-syntax-tree 5 grammar start-rule))
;;     ;expr
;; ;;;what we know
;;     (and (= 2 (eval (generate-syntax-tree 5 grammar start-rule) (get-current-environment))) (= 4 (eval (generate-syntax-tree 5 grammar start-rule) (get-current-environment))) (= 6 (eval (generate-syntax-tree 5 grammar start-rule) (get-current-environment))))
;;     ;(= 4 (eval expr (get-current-environment)))
;;     )
;;    )
;; (pretty-print samples)
;; (define (mappable-eval expression)
;;   (eval expression (get-current-environment)))
;; (map mappable-eval (map fourth samples))

;;;testing
;;;tree grammar tests
;; (define j (join '(3 4 (4 5)) 4))
;; (define k (join (list 3 4 (join 4 5)) 4))
;; (pretty-print j)
;; (pretty-print k)
;; (jlist? j)
;; (flatten-join j)
;; k
;; (flatten-join k)          
;(has-operands? ''())            
(define expr (generate-syntax-tree 10 trees 'T))
;expr
(eval '((lambda (x) (append '(node color) x)) ()) (get-current-environment))
(pretty-print expr)
(eval expr (get-current-environment))
;(all-empty? '((())))
    ;; ;; ;(generate-syntax-tree 10 naturals 'N)
    ;; ;;(generate-grammar naturals)
    
 )

(exit)
