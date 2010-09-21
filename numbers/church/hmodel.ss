(import (srfi :1 lists))
(import (church))
(church
 (define rule-names '(N))
 (define operators '());an assoc list of procedure names and definitions
 (define generate-grammar
   generate-rule '())
   ;(mem (lambda '())))
 ;(define (generate-parse grammar '())

 (define (test x y)
   (+ x y))

 (define (generate-operator grammar start)
   (let* ((expression (generate-expression grammar start))
          (procedure (define-procedure expression))
          (number-of-args (count-args procedure)) 
          (rule-names (repeat number-of-args random-existing-rule-name)))
     (pair procedure rule-names)))
 
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

