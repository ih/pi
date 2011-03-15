(library (abstraction-grammar)
         (export make-start-rule select apply-option make-rule START-SYMBOL make-grammar make-option rule->define)
         (import (rnrs)
                 (abstract)
                 (_srfi :1)
                 (only (church readable-scheme) uniform-draw))
;;;select will eventually take an optional parameter, this can vary depending on what select calls, e.g. if select is a multinomial than the optional parameter could be a list of probabilities
;;;select :: list -> item in the list
         (define START-SYMBOL 'S)
         
         (define select uniform-draw)

         (define (make-rule rule-name options)
           (list 'rule rule-name options))

         (define rule->name second)
         (define rule->options third)
         (define rule->options-list third)

         ;;turn a rule into a define
         (define (rule->define rule)
           `(define (,(rule->name rule)) (apply-option ,@(rule->options-list rule))))

         (define (make-option sexpr)
           `(lambda () ,sexpr))

         ;;every rule takes a list of sexpressions that will be the options for the rhs
         (define (make-start-rule rhs-sexprs)
           (make-rule START-SYMBOL rhs-sexprs))


         (define (apply-option . options)
           ((select options)))

         (define (make-grammar rule-list)
           `(let () ,@rule-list  (lambda () (,START-SYMBOL))))
         )



