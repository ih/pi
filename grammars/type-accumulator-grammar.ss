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


;;grammar rules, the parse includes its derivation, this is used in the definition of a function so that the types for the variables can be recorded
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
         (option IF)
         (option FUNC-APP))))


(define (NODE)
  (rule make-node
        (rhs (option LABEL TREE-EXPR))))

(define (IF)
  (rule make-if
        (rhs (option TREE-EXPR TREE-EXPR))))

(define (FUNC-APP)
  (rule make-application
             (rhs (option EXISTING-FUNCTION))))

(define (EXISTING-FUNCTION)
  (uniform-draw functions))

(define (FUNCTION)
  (rule make-function
        (rhs (option FUNC-LABEL))))

;;type constructors
;;additional information e.g. type, to be included with the expression
(define (extra-info expr type sub-expr-parses)
  (list expr type sub-expr-parses))
  
(define (make-label label-info)
  (let ([label (first label-info)])
    (extra-info label LABEL '())))

(define (make-tree-expr tree-info)
  (let ([expr (first tree-info)])
    (if (null? expr)
        (extra-info expr TREE-EXPR '())
        (first tree-info))))

(define (make-node node-info)
  (let* ([label (first (first node-info))]
         [expr (first (second node-info))]
         [db (pretty-print (list "mnode" node-info))])
    (if (null? expr)
        (extra-info (list 'node label) NODE (list label))
        (extra-info (list 'node label expr) NODE (list label expr)))))

(define (make-if if-info)
  (let* ([true-condition (first (first if-info))]
         [false-condition (first (second if-info))])
    (extra-info (list 'if '(flip) true-condition false-condition) IF (list true-condition false-condition))))

(define (make-application func-info)
  (let* ([name (first (first func-info))]
         [arg-types (second (first func-info))]
         [args (map first (map sample arg-types))])
    (extra-info (pair name args) FUNC-APP arg-types)))


;;;function definition related code
;;function information
(define functions (list (list 'F1 (list LABEL TREE-EXPR)) (list 'F2 '()) (list 'F3 (list LABEL LABEL))))


(define (make-function func-info)
  (let* ([name (first func-info)]
         [vars (second func-info)]
         [tree-expr (third func-info)]
         [body+var-types (insert-vars vars tree-expr)]
         [body (first body+var-types)]
         [var-types (second body+var-types)])
    (set! functions (append functions (list name var-types)))
    (list `(define (,name ,vars) ,body) FUNCTION)))

(define (insert-vars vars expr+)
  (define (insert-var var body+var-types)
    (let ([body (first body+var-types)]
          [var-types (second body+var-types)]
          [target-sexpr+ (random-subexpr body)]
          [type (return-type target-sexpr+)]
          [new-body (sexp-replace target-sexpr var body)])
      (list new-body (pair type vartypes))))
  (let ([expr (first expr+)])
    (fold insert-var (list expr '()) vars)))

;;inefficient to list out all the subexpressions w/ flatten...might want to do something more direct
(define (random-subexpr sexpr+)
  (uniform-draw (all-sub-exprs sexpr+)))

(define (all-sub-exprs sexpr)
  (if (list? sexpr)
      (pair sexpr (map all-sub-exprs sexpr))
      sexpr))

;;- correct last arg for extra-info in most cases to act upon x-info rather than on the expression extracted from parts of x-info
;;- write FUNC-LABEL
;;- write random-subexpr

;;- write all-sub-exprs
;;- write return-type, matches on pattern in type-constructor
;;- test insert-vars
;;- write FNAME
;;- write make-function
;;- write rule VARS
;;- write rule to generate function definitions
