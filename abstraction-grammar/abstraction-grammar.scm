(library (abstraction-grammar)
         (export make-start-rule select apply-option make-rule START-SYMBOL make-grammar make-option rule->define make-options grammar->sexpr)
         (import (rnrs)
                 (abstract)
                 (_srfi :1)
                 (only (church readable-scheme) uniform-draw pair))
;;;select will eventually take an optional parameter, this can vary depending on what select calls, e.g. if select is a multinomial than the optional parameter could be a list of probabilities
;;;select :: list -> item in the list
         (define START-SYMBOL 'S)
         
         (define select uniform-draw)

         ;;options are sexprs for thunks
         (define (make-rule rule-name options)
           (list 'rule rule-name options))

         (define rule->name second)
         (define rule->options third)
         (define rule->options-list third)

         ;;get the sexpr representation of a rule
         (define (rule->define rule)
           `(define (,(rule->name rule)) (apply-option ,@(rule->options-list rule))))

         (define (make-option sexpr)
           `(lambda () ,sexpr))

         (define (make-options sexpr-list)
           (map make-option sexpr-list))

         ;;every rule takes a list of sexpressions that will be the options for the rhs
         (define (make-start-rule options)
           (make-rule START-SYMBOL options))

         (define (apply-option . options)
           ((select options)))

         (define (make-grammar start-rule rule-list)
           (list 'grammar (rule->name start-rule) (pair start-rule rule-list)))

         (define grammar->rule-list third)

         (define grammar->start-symbol second)
         
         (define (grammar->sexpr grammar)
           `(let () ,@(map rule->define (grammar->rule-list grammar))  (lambda () (,(grammar->start-symbol grammar)))))
         )



