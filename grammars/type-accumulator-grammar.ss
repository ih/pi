;;this generative grammar maintains the type with each evaluation of the rule along with the types of the subexpressions
;;e.g. (LABEL) => ('a LABEL)
;; (NODE) => ((node a) NODE (('a LABEL) ('() TREE-EXPR)))

;;TO DO
;;- write insert-vars to use the type information 
;;- write rule to generate programs (definitions and a body)
(import (except (rnrs) string-hash string-ci-hash)
        (only (ikarus) set-car! set-cdr!)
        (_srfi :1)
        (_srfi :69)
        (church)
        (church readable-scheme))
;;general functions
(define (sexp-replace old new sexp)
  (if (list? sexp)
      (map (curry sexp-replace old new) sexp)
      (if (equal? sexp old) new sexp)))


;;related to rules
(define (sample distribution)
  (apply distribution '()))

(define (terminal t) (lambda () t))

(define (rule constructor rhs)
  (constructor (map sample (uniform-draw rhs))))

(define rhs list)
(define option list)


;;grammar rules
(define (LABEL)
  (rule make-label
        (rhs (option (terminal 'a))
             (option (terminal 'b))
             (option (terminal 'c)))))


(define (TREE-EXPR)
  (rule make-tree-expr
        (rhs 
         (option NODE)
         (option (terminal '()))
         (option IF))))
;;(option FUNC-APP)


(define (NODE)
  (rule make-node
        (rhs (option LABEL TREE-EXPR))))

(define (IF)
  (rule make-if
        (rhs (option TREE-EXPR TREE-EXPR))))

(define (FUNC-APP)
  (rule make-application
             (rhs (option FUNCTION))))

(define (FUNCTION)
  (uniform-draw functions))





;;type constructors
(define (make-label label-info)
  (let ([label (first label-info)])
    (list label LABEL)))

(define (make-tree-expr tree-info)
  (if (null? (first tree-info))
      (list (first tree-info) TREE-EXPR)
      (list (first (first tree-info)) TREE-EXPR)))

(define (make-node node-info)
  (let* ([label (first (first node-info))]
         [expr (first (second node-info))])
    (if (null? expr)
        (list (list 'node label) NODE)
        (list (list 'node label expr) NODE))))

(define (make-if if-info)
  (let* ([true-condition (first (first if-info))]
         [false-condition (first (second if-info))])
    (list (list 'if '(flip) true-condition false-condition) IF)))

(define (make-application func-info)
  (let ([name (first (first func-info))]
        [args (second (first func-info))])
    (list (pair name (map first (map sample args))) FUNC-APP)))



;;function information
(define functions (list (list 'F1 (list LABEL TREE-EXPR)) (list 'F2 '()) (list 'F3 (list LABEL LABEL))))


(define (make-function func-info)
  (let* ([name (first func-info)]
         [vars (second func-info)]
         [tree-expr (third func-info)]
         [body (insert-vars vars tree-expr)])
    `(define (,name ,vars) ,body)))

(define (insert-vars vars body)
  (define (insert-var var body-types)
    (let ([body (first body-types)]
          [types (second body-types)]
          [target-sexpr (random-subexpr body)]
          [type (return-type replaced-sexpr)]
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



;;- write all-sub-exprs
;;- write random-subexpr          
;;- write return-type, matches on pattern in type-constructor
;;- test insert-vars
;;- write FNAME
;;- write make-function
;;- write rule VARS
;;- write rule to generate function definitions
