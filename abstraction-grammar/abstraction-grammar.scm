(library (abstraction-grammar)
         (export make-start-rule select rhs-options apply-option make-rule START-SYMBOL make-grammar)
         (import (rnrs)
                 (abstract)
                 (only (church readable-scheme) uniform-draw))
;;;select will eventually take an optional parameter, this can vary depending on what select calls, e.g. if select is a multinomial than the optional parameter could be a list of probabilities
;;;select :: list -> item in the list
         (define START-SYMBOL 'S)
         
         (define select uniform-draw)

         (define rhs-options list)

         (define (make-option sexpr)
           `(lambda () ,sexpr))

         ;;every rule takes a list of sexpressions that will be the options for the rhs
         (define (make-start-rule rhs-sexprs)
           (make-rule START-SYMBOL rhs-sexprs))

         (define (make-rule rule-name rhs-sexprs)
           `(define (,rule-name) (apply-option ,@(map make-option rhs-sexprs))))

         (define (apply-option . options)
           ((select options)))

         (define (make-grammar rule-list)
           `(let () ,@rule-list  (lambda () (,START-SYMBOL))))
         )



