;;;TO DO
;;-test (+ 2 2), ((lambda (x) (+ x 2)) 3) 
;;-add unless to the language

(import (srfi :1)
        (only (util) rest pair))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(unless? exp) (analyze-unless exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(application? exp) (analyze-application exp)]
        [(variable? exp) (analyze-variable exp)]
        [(unless? exp) (analyze-unless exp)]
;;        [(unless? exp) (analyze (unless->applied-if-with-thunks exp))]

        ;; [(quoted? exp) (analyze-quoted exp)]
        ;; [(assignment? exp) (analyze-assignment exp)]
        ;; [(definition? exp) (analyze-definition exp)]
        ;; [(begin? exp) (analyze-begin exp)]
        ;; [(cond? exp) (analyze-cond exp)]
        [else (error "ANALYZE" "Unknown expression type" exp)]))
;;(pretty-print "not in language")
;;

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (first exp) tag)
      #f))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;VARIABLES
(define variable? symbol?)

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (lookup-variable-value variable environment)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? (first vars) variable) (first vals)]
            [else (scan (rest vars) (rest vals))]))
    (if (eq? env the-empty-environment)
        (error "scan" "unbound variable" variable)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop environment))

;;;PROCEDURES
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define procedure-parameters second)

(define procedure-body third)

(define procedure-environment fourth)

(define (primitive-procedure? exp)
  (tagged-list? exp 'primitive))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define primitive-implementation second)

(define primitive-procedures
  (list (list 'first first)
        (list 'rest rest)
        (list '+ +)
        (list '- -)
        (list 'pair pair)
        (list 'null? null?)
        (list '= =)
        (list '/ /)))

(define (primitive-procedure-names) (map first primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (primitive-implementation proc))) primitive-procedures))
  


;;;SELF-EVALUATING
(define (self-evaluating? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;;;IF 
(define (if? exp)
  (tagged-list? exp 'if))

(define if-predicate second)

(define if-consequent third)

(define if-alternative fourth)

(define (analyze-if exp)
  (let ([predicate-proc (analyze (if-predicate exp))]
        [consequent-proc (analyze (if-consequent exp))]
        [alternative-proc (analyze (if-alternative exp))])
    (lambda (env)
      (if (true? (predicate-proc env))
          (consequent-proc env)
          (alternative-proc env)))))



;;;LAMBDA
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define lambda-parameters second)

(define (lambda-body exp) (rest (rest exp)))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [body-proc (analyze-sequence (lambda-body exp))])
    (lambda (env) (make-procedure vars body-proc env))))
        

;;;UNLESS

;;;UNLESS
;;from lawful samurai
(define (unless? exp)
  (tagged-list? exp 'unless))

(define unless-condition second)
(define unless-usual-value third)
(define unless-exceptional-value fourth)

(define (analyze-unless exp)  
  (let ((condition (analyze (unless-condition exp)))  
        (usual-value (analyze (unless-usual-value exp)))  
        (exceptional-value (analyze (unless-exceptional-value exp))))
    (lambda (env)  
      (if (true? (condition env))  
          (exceptional-value env)  
          (usual-value env)))))

;;end lawful samurai


;; (define (unless? exp)
;;   (tagged-list? exp 'unless))

;; (define (unless-predicate exp)
;;   (second exp))

;; (define (unless-usual exp)
;;   (third exp))

;; (define (unless-exception exp)
;;   (fourth exp))

;; (define (unless->applied-if-with-thunks exp)
;;   (let ([predicate (unless-predicate exp)]
;;         [usual-thunk (make-thunk (unless-usual exp))]
;;         [exception-thunk (make-thunk (unless-exception exp))])
;;     (make-application (make-if predicate usual-thunk exception-thunk))))

;;;SEQUENCE

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (first rest-procs)) (rest rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs)
        (error "ANALYZE SEQUENCE" "EMPTY SEQUENCE - procs" exps)
        (loop (first procs) (rest procs)))))

;;;APPLICATION
(define application? pair?)

(define (analyze-application exp)
  (let ([operator-proc (analyze (operator exp))]
        [operand-procs (map analyze (operands exp))])
    (lambda (env)
      (execute-application (operator-proc env)
                           (map (lambda (operand-proc) (operand-proc env)) operand-procs)))))

;;used at run-time
(define (execute-application proc args)
  (cond [(primitive-procedure? proc)
         (apply-primitive-procedure proc args)]
        [(compound-procedure? proc)
         ;;procedure-body is an analyzed expression (it's a lambda function that takes an environment)
         ((procedure-body proc) (extend-environment (procedure-parameters proc) args (procedure-environment proc)))]
        [else (error "application execution" "unknown procedure type" proc)]))

(define (apply-primitive-procedure proc args)
    (underlying-apply (primitive-implementation proc) args))

(define underlying-apply apply)

(define operator first)

(define operands rest)



;;;ENVIRONMENT
(define (extend-environment variables values environment)
  (if (= (length variables) (length values))
      (let ([new-frame (make-frame variables values)])
        (pair new-frame environment))
      (error "extend-environment" "number of variables and values don't match" (list (length variables) (length values)))))
        
(define (make-frame variables values)
  (pair variables values))

(define enclosing-environment rest)

(define the-empty-environment '())

(define first-frame first)

(define frame-variables first)

(define frame-values rest)

(define (setup-environment)
  (let ([initial-environment (extend-environment (primitive-procedure-names)
                                                 (primitive-procedure-objects)
                                                 the-empty-environment)])
    initial-environment))

(define the-global-environment (setup-environment))

        
    
;;;TESTING
;;(pretty-print (eval '(if #f #f #t) the-global-environment))
;;(pretty-print (eval '(+ 2 2) the-global-environment))
;;(pretty-print (eval '((lambda (x) (+ x 2)) 4) the-global-environment))
(pretty-print (eval '(unless (= 0 0) (/ 4 0) 999) the-global-environment))








