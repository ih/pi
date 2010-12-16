(define MAX-FUNCS 5)
(define MAX-ARITY 3)
;;grammar for generating programs that consist of a list of definitions and a body that may use those definitions

(define (gen-program)
    (let* ([num-of-funcs (random-integer MAX-FUNCS)]
         [function-signatures (gen-func-signatures num-of-funcs)]
         [functions (map (curry gen-function function-signatures) function-signatures)]
         [body (gen-expr '() function-signatures)])
    (combine-program-parts functions body)))
(define (combine-program-parts functions body)
    (append (append (list 'let '()) functions) (list body)))

;;function signature related functions
(define get-name first)
(define get-vars rest)
(define (get-arity signature)
  (length (get-vars signature)))

(define (gen-func-signatures num-of-funcs)
  (repeat num-of-funcs gen-func-signature))


(define (gen-func-signature)
  (let* ([name (sym 'F)]
         [arity (random-integer MAX-ARITY)]
         [vars (gen-vars arity)])
    (combine-sig-parts name vars)))
(define combine-sig-parts pair)

(define (gen-vars arity)
  (repeat arity (lambda () (sym 'V))))

         
;;creates a function based on function-signature and may use functions specified by function-signatures in the body
(define (gen-function function-signature function-signatures)
  (let* ([function-signatures (remove function-signature function-signatures)] ;;ensures no recursive functions
         [vars (get-vars function-signature)]
         [body (gen-expr vars function-signatures)])
    (combine-function-parts function-signature body)))
(define (combine-function-parts function-signature body)
  (list 'define function-signature body))

(define (gen-expr vars function-signatures)
  (if (null? vars)
      (multinomial (list (gen-if vars function-signatures) (gen-bool) (gen-application vars function-signatures)) (list 1/8 3/4 1/8))
      (multinomial (list (gen-if vars function-signatures) (gen-bool) (gen-application vars function-signatures) (gen-var vars))) (list 1/12 3/4 1/12 1/12)))

(define (gen-bool)
  (uniform-draw '(t f)))

(define (gen-if vars function-signatures)
  (combine-if-parts 'if (gen-expr vars function-signatures) (gen-expr vars function-signatures) (gen-expr vars function-signatures)))
(define combine-if-parts list)

(define (gen-application vars function-signatures)
  (let* ([function-signature (uniform-draw function-signatures)]
         [function (get-name function-signature)]
         [arity (get-arity function-signature)]
         [args (gen-args arity)])
    (combine-application-parts function args)))
(define combine-application-parts pair)
(define (gen-args arity)
  (repeat arity (lambda () (gen-expr vars function-signatures))))

(define (gen-var vars)
  (uniform-draw vars))

(pretty-print (gen-program))
