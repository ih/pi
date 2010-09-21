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

 (define (curry fun . args)
          (lambda x
            (apply fun (append args x))))
 ((curry * 2) 6)

 (define (generate-expression grammar rule-name)
   (let* ((current-rule (select-rule grammar rule-name))
          (node (choose-node current-rule)))
     (if (has-children? node)
         (pair (get-operator-name node) (map (curry generate-expression grammar) (get-children node)))
         node)))

;; (define (tree-recursion interior-function leaf-function node)
;;   (if (has-children? node)
;;       (let ((children (get-children node))
;;             (tree-recurse (curry tree-recursion interior-function leaf-function)))
;;         (interior-function node (map tree-recurse children)))
;;       (leaf-function node)))

 
 ;; (define (generate-expression grammar rule-name)
 ;;  (tree-recursion
 ;;     (lambda (rule-name children)
 ;;       (let* ((current-rule (select-rule grammar rule-name))
 ;;              (node (choose-node current-rule)))
 ;;         (pair (get-operator-name node) children)))
 ;;     (lambda (rule-name)
 ;;       (let* ((current-rule (select-rule grammar rule-name))
 ;;              (node (choose-node current-rule)))
 ;;         node)) rule-name))

 
 (define texp (generate-expression base 'N))
;  (pretty-print texp)
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

; (define num-vars (count-leaves texp))



;; (define (count-leaves root) (tree-recursion
;;   (lambda (node children) (apply + children))
;;   (lambda (node) 1) root))
;; (define num-vars (count-leaves texp))

;; (define (insert-variables expression)
;;   (let* ((upper-bound (count-leaves expression))
;;          (possible-variables (list-possible-variables upper-bound)))
;;     (tree-recursion
;;      (lambda (expression children) (pair (get-operator-name expression) children))
;;      (lambda (expression) (if (flip)
;;                               (uniform-draw possible-variables)
;;                               expression)) expression)))
;; (insert-variables '(+ 1 (+ 1 1)))

 
;;  (define tvars (list-possible-variables num-vars))
 


 
 (define (insert-variables-recursion variables expression)
   (if (has-children? expression)
       (let ((children (get-children expression)))
         (pair (get-operator-name expression) (map (curry insert-variables-recursion variables) children)))
       (if (flip)
           (uniform-draw variables)
           expression)))
 

 (insert-variables-recursion '(a b c) '(+ 1 (+ 1 1)))


 (define (insert-variables expression)
  (let* ((upper-bound (count-leaves expression))
         (possible-variables (list-possible-variables upper-bound)))
    (insert-variables-recursion possible-variables expression)))
 
 (define vexp (insert-variables texp))

; (pretty-print vexp)
 (define variable? symbol?)
 (variable? 3)
 (define (get-variables-recursion body)
   (if (has-children? body)
       (let ((children (get-children body)))
         (append (map get-variables-recursion children)))
       (if (variable? body)
           body
           '())))

 (define (flatten l)
  (cond ((null? l) '())
        ((list? l)
         (append (flatten (first l)) (flatten (rest l))))
        (else (list l))))
 
(flatten (get-variables-recursion '(+ 1 (+ 'c 1))))


(define (member? item lst)
  (if (null? lst)
      false
      (if (equal? item (first lst))
          true
          (member? item (rest lst)))))

(member? 3 '(1 2 5))

(define (delete-duplicates-helper set lst)
  (if (null? lst)
      set
      (delete-duplicates-helper
       (if (member? (first lst) set)
           set
           (pair (first lst) set))
       (rest lst))))
(define (delete-duplicates lst)
  (delete-duplicates-helper '() lst))

(delete-duplicates '(1 2 3 2 1))

 (define (get-variables body)
   (delete-duplicates (flatten (get-variables-recursion body))))
(get-variables vexp)

 (define (create-head body)
   (let ((variable-names (get-variables body)))
     (list 'lambda variable-names)))

;; (define (create-head body)
;;    (let ((variable-names (get-variables body)))
;;      (list 'define (pair (gen-sym) variable-names))))

(create-head vexp)

(create-head '(+ 'a 'a))

 (define (define-procedure expression)
   (let* ((body (insert-variables expression))
          (head (create-head body)))
     (append head (list body))))

(define (get-procedure-name procedure)
  (first (second procedure)))

(define p (define-procedure '(+ 1 (+ 1 1))))
(pretty-print p)
(define (get-body procedure)
  (third procedure))

(get-body p)


(define (count-args procedure)
  (let ((variables (second procedure)))
    (length variables)))

(count-args p)
(define rule-names '(N E))
(define (random-existing-rule-name)
  (uniform-draw rule-names))

(random-existing-rule-name)



;(eval '((lambda (g1 g2) (+ (+ (+ g2 g1) 1) 1)) 4 3) (get-current-environment))

(define choose-rule-name uniform-draw)

(choose-rule-name '(a b c d))
(define (get-rule-names grammar)
  (map first grammar))

(get-rule-names base)
(uniform-draw base)
 (define (generate-operator grammar rule-name)
   (let* ((start-rule (uniform-draw grammar))
          (expression (generate-expression grammar (get-rule-name start-rule)))
          (procedure (define-procedure expression))
          (number-of-args (count-args procedure))
          (possible-names (pair rule-name (get-rule-names grammar))) 
          (rule-names (repeat number-of-args (curry choose-rule-name possible-names))))
     (pair procedure rule-names)))

 (define (sample-positive-integer n)
   (+ (sample-integer n) 1))

(generate-operator base 'E)
 (define max-rhs-length 5)
 (define (generate-rhs grammar name)
   (let ((rhs-length (sample-positive-integer max-rhs-length)))
     (repeat rhs-length (curry generate-operator grammar name))))

(generate-rhs base 'E)
;(eval ((lambda () 1)) (get-current-environment))
;(repeat 0 (lambda () 1))
)
(exit)