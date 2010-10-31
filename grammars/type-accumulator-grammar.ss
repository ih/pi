;;this generative grammar maintains the type with each evaluation of the rule along with the types of the subexpressions
;;e.g. (LABEL) => ('a LABEL)
;; (NODE) => ((node a) NODE (('a LABEL) ('() TREE-EXPR)))

;;TO DO
;; -write NULL and integrate it into DEFINITIONS and TREE-EXPR
;;- write a function that allows for repeated variables by collapsing what is returned by choose-insertion-points
;; -change insert-variables and choose-subexprs to only replace the instance of the chosen subexpression where it was chosen...currently replaces all instances of the chosen subexpression
;;- figure out how to get functions w/ different return types
;;- add flag to rule function so that parse info is only tracked when called FUNCTION rule i.e. only track parse info when building the body of a new function
(import (except (rnrs) string-hash string-ci-hash)
        (only (ikarus) set-car! set-cdr!)
        (_srfi :1)
        (_srfi :69)
        (sym)
        (util)
        (church)
        (church readable-scheme))
;;general functions
(define (multinomial values distribution)
  (let* ([u (random-real)]
         [bin (find-bin u distribution)])
    (list-ref values bin)))

(define (find-bin u distribution)
  (let loop ([bin 0]
             [accum (list-ref distribution 0)])
    (if (<= u accum)
        bin
        (loop (+ bin 1) (+ accum (list-ref distribution (+ bin 1)))))))

;;related to rules
(define (sample distribution)
  (apply distribution '()))

(define (terminal t) (lambda () t))

(define (rule constructor rhs . mvalues)
  (if (null? mvalues)
      (constructor (map sample (uniform-draw rhs)))
      (constructor (map sample (multinomial rhs mvalues)))))

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
         (option (terminal '()))
         (option NODE)
         (option IF)
         (option FUNC-APP)) .3 (/ .7 3) (/ .7 3) (/ .7 3)))

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
  (if (null? functions)
      '()
      (uniform-draw functions)))

;;;the rules below do not need to keep track of the complete parse so they don't use extra-info in their constructors
(define (PROGRAM)
  (rule make-program
        (rhs
         (option DEFINITIONS TREE-EXPR))))

(define (DEFINITIONS)
  (rule make-defs
        (rhs (option FUNCTION DEFINITIONS)
             (option (terminal '())))))

(define (FUNCTION)
  (rule make-function
        (rhs (option (terminal (sym 'F)) TREE-EXPR))))

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
  (let* ([label-parse  (first node-info)]
         [expr-parse (second node-info)]
         [label  (first label-parse)]
         [expr (first expr-parse)])
    (if (null? expr)
        (extra-info (list 'node label) NODE (list label-parse))
        (extra-info (list 'node label expr) NODE (list label-parse expr-parse)))))

(define (make-if if-info)
  (let* ([true-condition-parse (first if-info)]
         [false-condition-parse (second if-info)]
         [true-condition (first true-condition-parse)]
         [false-condition (first false-condition-parse)])
    (extra-info (list 'if '(flip) true-condition false-condition) IF (list true-condition-parse false-condition-parse))))

(define (make-application func-info)
  (if (null? (first func-info))
      '()
      (let* ([name (first (first func-info))]
             [arg-types (second (first func-info))]
             [arg-parses (map sample arg-types)]
             [arg-exprs (map first arg-parses)])
        (extra-info (pair name arg-exprs) FUNC-APP arg-parses))))

;;doesn't use extra-info because definitions don't appear in the body of functions 
(define (make-defs defs-info)
  (if (null? (first defs-info))
      '()
      (let* ([func-parse (first defs-info)]
             [func-list (second defs-info)])
        (pair (first func-parse) func-list))))


(define (make-program prog-info)
  (set! functions '())
  (let* ([defs (first prog-info)]
         [expr-parse (second prog-info)])
    (append '(let ()) defs (list (first expr-parse)))))
         

;;;function definition related code
;;function information
;;(define functions (list (list 'F1 (list LABEL TREE-EXPR)) (list 'F2 '()) (list 'F3 (list LABEL LABEL))))
(define functions '())

(define (make-function func-info)
  (let* ([name (first func-info)]
         [expr-parse (second func-info)]
         [var+type+subexprs (choose-subexprs expr-parse)]
         [vars (map first var+type+subexprs)]
         [var+subexprs (zip vars (map third var+type+subexprs))]
         [body (insert-variables var+subexprs (first expr-parse))]
         [var-types (map second var+type+subexprs)])
    (set! functions (append functions (list (list name var-types))))
    (list (list 'define (pair name vars) body))))

;;return list of (var-name type subexpr) to be used in replacing vars
(define (choose-subexprs expr-parse)
  ;;internal definition so that we don't choose the whole expression as the subexpression
  (define (choose-sub-exprs expr-parse)
    (if (flip)
        (let ([expr (first expr-parse)]
              [type (second expr-parse)])
          (list (list (sym 'V) type expr)))
        (apply append (map choose-sub-exprs (third expr-parse)))))
  (apply append (map choose-sub-exprs (third expr-parse))))
  

;;insert variables into the original expression
(define (insert-variables var+subexprs expr)
  (define (insert-variable var+subexpr expr)
    (let* ([var (first var+subexpr)]
           [sub-expr (second var+subexpr)])
      (sexp-replace sub-expr var expr)))
  (fold insert-variable expr var+subexprs))





