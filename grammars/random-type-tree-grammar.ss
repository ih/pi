;;this generative grammar creates variables by randomly choosing a type and listing all the expressions that match that type in the body and chooses randomly from these, this is interesting b/c one can think of the process of finding the expressions that match the type as object recognition, functions developed here may be useful when looking at learning grammars and applying them

;;TO DO
;;resolve the difference between pattern matching in the case for "regular" abstractions and recursion...read wikipedia page on fixed point combinators
;;write find-all-types
;;write random-type which chooses a rule randomly
;;- move appropriate functions to abstract.ss e.g. abstraction-matches, instantiate etc.
(import (_srfi :1)
        (church)
        (church readable-scheme))

;;general functions
(define (sexp-replace old new sexp)
  (if (list? sexp)
      (map (curry sexp-replace old new) sexp)
      (if (equal? sexp old) new sexp)))


(define (sample distribution) (apply distribution '()))

(define (terminal t) (lambda () t))


;;grammar rules
(define FUNCTION
  (lambda ()
    (make-function (map sample (list FNAME VARS TREE-EXPR)))))

(define LABEL
  (lambda ()
    (sample 
    (uniform-draw
     (list
      (terminal 'a)
      (terminal 'b)
      (terminal 'c))))))

(define TREE-EXPR
  (lambda ()
    (sample (uniform-draw (list IF NODE FUNC-APP (terminal '()))))))


(define FUNC-APP
  (lambda ()
    (make-application (uniform-draw functions))))

(define IF
  (lambda ()
    (make-if (map sample (list TREE-EXPR TREE-EXPR)))))


(define NODE
  (lambda ()
    (make-node (map sample (list LABEL TREE-EXPR)))))

;;function information
(define functions (list (list 'F1 (list LABEL TREE-EXPR)) (list 'F2 '()) (list 'F3 (list LABEL LABEL))))


;;"type constructors" functions for making the expression forthe corresponding rule
(define (make-application func-info)
  (let ([name (first func-info)]
        [args (second func-info)])
    (pair name (map sample args))))
    
(define (make-if if-info)
  (let ([true-condition (first if-info)]
        [false-condition (second if-info)])
    `(if (flip) ,true-condition ,false-condition)))

(define (make-node node-info)
  (let* ([label (first node-info)]
        [expr (second node-info)])
;;        [db (pretty-print (list "expr" expr))])
    (if (null? expr)
        (list 'node label)
        (list 'node label expr))))

(define (make-function func-info)
  (let* ([name (first func-info)]
         [vars (second func-info)]
         [body (third func-info)]
         [body-with-vars (insert-vars vars body))
)))

(define (insert-vars vars body)
  (define (insert-var var body-types)
    (let ([body (first body-types)]
          [types (second body-types)]
          [target-type (uniform-draw types)]
          [type-instances (type-matches body target-type)]
          [target-sexpr (uniform-draw type-instances)]
          [new-body (sexp-replace target-sexpr var body)])
      (list new-body (pair type types))))
  (fold insert-var (list body '()) vars))

;;inefficient to list out all the subexpressions w/ flatten...might want to do something more direct
(define (random-subexpr sexpr)
  (uniform-draw (all-sub-exprs sexpr)))

(define (all-sub-exprs sexpr)
  (if (list? sexpr)
      (pair sexpr (map all-sub-exprs sexpr))
      sexpr))

;;return all the subexpressions that have type type
(define (type-matches s type)
  (let ([abstractions (type->abstractions type)])
    (map (curry abstraction-matches s) abstractions)))

;;;CODE BELOW SHOULD BE MERGED INTO ABSTRACT.SS
;;return all instances of abstraction in s
(define (abstraction-matches s abstraction)
  (let ([variable-values (unify-all s abstraction)])
    (map (curry instantiate abstraction) variable-values)))

;;this is similar to replace-matches
(define (unify-all s abstraction)
  (fold unify 


;;take values for the variables of an abstraction and return the completed pattern DELETE THESE WHEN NOT NEEDED
(define (instantiate abstraction variable-values)
  (let ([pattern (abstraction->pattern abstraction)])
    (fold instantiate-variable pattern variable-values)))

(define (instantiate-variable variable-value pattern)
  (let* ([name (first variable-value)]
         [value (second variable-value)])
    (sexp-replace name value pattern)))


;;-write abstraction-matches as replace-matches, don't use unify approach (finding all values then instantiating...)
;;- write a version of unify that does not try to check/remove-repeated variables
;;- test type-matches
;;- write insert-vars to use the random type method
;;- rewrite rules in terms of abstractions, call them types

;;- write FNAME
;;- write make-function
;;- write rule VARS
;;- write rule to generate function definitions
