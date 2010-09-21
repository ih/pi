(import (srfi :1 lists))
(import (church))
(church
 ;; (define rule-names '(A B C D E F G H I J))
 (define max-number-rules 10)
 (define max-rhs-length 5)


 
 (define (generate-grammar start-grammar)
   (let* ((number-of-rules (sample-integer max-number-rules))
          (rule-names (make-rule-names number-of-rules))
     (generate-rules start-grammar rule-names))))

 (define (generate-rules grammar future-names)
   (if (null? future-names)
       grammar
       (let ((new-rule (generate-rule grammar (first future-names))))
         (generate-rules (add-rule new-rule grammar) (rest possible-names)))))

 (define (generate-rule grammar name old-names)
   (let ((rhs (create-rhs grammar name)))
     (ensure-non-circular (pair name rhs))))

 (define (generate-rhs grammar name)
   (let* ((rhs-length (sample-positive-integer max-rhs-length)))
     (repeat rhs-length (curry generate-operator grammar name))))

 (define (sample-positive-integer n)
   (+ (sample-integer n) 1))
   ;this version might result in loops, you have to be careful in how you insert rule names to the rhs, need to always ensure a path to the root
 ;; (define (generate-rules current-grammar rule-names)
 ;;   (map (curry generate-rule current-grammar rule-names) rule-names))

 ;; (define (generate-rule grammar possible-rules rule-name)
 ;;   (let* ((rhs-length (sample-integer max-rhs-legth))
 ;;          (rhs (repeat rhs-length (curry generate-operator grammar))))
 ;;     (pair rule-name rhs)))

;change so start is removed and a random rule from the grammar is chonsen
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
 
 ;; (define (generate-operator grammar start)
 ;;   (let* ((expression (generate-expression grammar start))
 ;;          (procedure (define-procedure expression))
 ;;          (procdeure-info (add-to-library procedure)) ;info is name and number of arguments
 ;;          (rule-names (repeat (number-of-args procedure-info) random-existing-rule-name)))
 ;;     (pair (name procedure-info) rule-names)))
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
     (append head (list body))))


 (define (insert-variables expression)
   (let* ((upper-bound (count-leaves expression))
          (possible-variables (list-possible-variables upper-bound)))
     (insert-variables-recursion possible-variables expression)))

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
     (list 'lambda variable-names))))

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
  (delete-duplicates-helper set lst))

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

(define (random-existing-rule-name)
  (uniform-draw rule-names))

;;;begin define-and-add-to-environment
(define (define-and-add-to-environment procedure)
  (let ((name (get-procedure-name procedure)))
    (begin
      (eval procedure (get-current-environment))
      name)))

(define (get-procedure-name procedure)
  (first (second procedure)))


;;;functions refactored with tree-recursion             
;; (define (tree-recursion interior-function leaf-function node)
;;   (if (has-children? node)
;;       (let ((children (get-children node))
;;             (tree-recurse (curry tree-recursion interior-function node-function)))
;;         (interior-function node (map tree-recurse children)))
;;       (leaf-function node)))

;; (define (count-leaves root) (tree-recursion
;;   (lambda (node children) (apply + children))
;;   (lambda (node) 1) root))

;; (define (insert-variables expression)
;;   (let* ((upper-bound (count-leaves expression))
;;          (possible-variables (list-possible-variables upper-bound)))
;;     (tree-recursion
;;      (lambda (expression children) (pair (get-operator-name expression) children))
;;      (lambda (expression) (if (flip)
;;                               (uniform-draw possible-variables)
;;                               expression)) expression)))

;; (define (generate-expression grammar rule-name)
;;   (tree-recursion
;;      (lambda (rule-name children)
;;        (let* ((current-rule (select-rule grammar rule-name))
;;               (node (choose-node current-rule)))
;;          (pair (get-operator-name node) children)))
;;      (lambda (rule-name)
;;        (let* ((current-rule (select-rule grammar rule-name))
;;               (node (choose-node current-rule)))
;;          node))))

         
)

