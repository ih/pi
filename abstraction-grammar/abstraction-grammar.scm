(library (abstraction-grammar)
         (export make-start-rule select apply-option make-rule START-SYMBOL make-grammar make-option rule->define make-options grammar->sexpr create-rule-applications body->options program->start-rule variable->rule abstraction->rules rule-application? program->rules program->grammar)
         (import (rnrs)
                 (abstract)
                 (_srfi :1)
                 (only (church readable-scheme) uniform-draw pair rest)
                 (util))
;;;select will eventually take an optional parameter, this can vary depending on what select calls, e.g. if select is a multinomial than the optional parameter could be a list of probabilities
;;;select :: list -> item in the list
         (define START-SYMBOL 'S)
         
         (define select uniform-draw)

         ;;the rule-name corresponds w/ the lhs of a production rule and the options are the possible expansions of the rule i.e. they make up the rhs of a production rule
         (define (make-rule rule-name options)
           (list 'rule rule-name options))

         (define rule->name second)
         (define rule->options third)
         ;;since options is implmented as a list rule->options-list merely returns options, but this abstraction exists in case the underlaying data structure for options changes
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
;;;conversion from programs found by abstraction into grammars
         (define (program->grammar program)
           (let* ([start-rule (program->start-rule program)]
                  [rules (program->rules program)])
            (make-grammar start-rule rules)))


         (define (program->start-rule program)
           (let* ([options (body->options (program->body program))])
             (make-start-rule options)))

         ;;assumes program has a body which is some sort of function over a list of sexprs e.g. uniform-draw
         ;;the sexprs that are abstraction function applications are returned as thunk applications
         ;;everything else is returned as is
         (define (body->options program-body)
           (let* ([sexprs (rest (second program-body))]
                  [thunkified-sexprs (create-rule-applications sexprs)])
             (make-options thunkified-sexprs)))

         (define (create-rule-applications sexprs)
           (define (create-rule-application sexpr)
             (if (list? sexpr)
                 (list (first sexpr))
                 (list sexpr)))
           (sexp-search rule-application? create-rule-application sexprs))

         ;;assumes abstractions and only abstractions have name of the form '[FUNC-SYMBOL][Number]
         (define (rule-application? sexpr)
           (if (non-empty-list? sexpr)
               (func? (first sexpr))
               (var? sexpr)))

         ;;abstraction->rules returns a list of rules because it returns both the rule for the abstraction function
         ;;as well as rules for any variables of the abstraction function
         (define (program->rules program)
           (let* ([abstractions (program->abstractions program)])
             (concatenate (map (curry abstraction->rules program) abstractions))))

         ;;consider making a special function make-abstraction-rule that calls make-rule, the difference would be it takes only a single option rather than a list of options
         (define (abstraction->rules program abstraction)
           (let* ([name (abstraction->name abstraction)]
                  [option (make-options (list (create-rule-applications (abstraction->pattern abstraction))))]
                  [variable-rules (map (curry variable->rule program abstraction) (abstraction->vars abstraction))])
             (pair (make-rule name option) variable-rules)))

         ;;need program to find all applications of the abstraction (e.g. '(F1 a b c) '(F1 1 3 a) would be applications of F1
         ;;need abstraction to know which argument variable is in the in the applications (and to find the applications)
         ;;if the signature for the abstraction is '(F1 V1 V2 V3) and the target variable is V2 then we'll make 'b and 3 options
         ;;eventually we can make this more compact be removing duplicate instances and drawing from a multinomial where the probability is based on the number of instances (but perhaps we should allow recursive compressions to deal with this?)
         ;;there are subtleties between number of applications and number of recursive applications of a function...maybe not
         (define (variable->rule program abstraction variable)
           (let* ([abstraction-applications (program->abstraction-applications program abstraction)]
                  [variable-position (abstraction->variable-position abstraction variable)]
                  [variable-instances (create-rule-applications (map (curry ith-argument variable-position) abstraction-applications))])
             (make-rule variable (make-options variable-instances))))

         ;;i+1 because the first element is the function name
         (define (ith-argument i function-application)
           (list-ref function-application (+ i 1))))








