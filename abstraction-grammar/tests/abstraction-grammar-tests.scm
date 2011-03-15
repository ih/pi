(import (rnrs)
        (abstract)
        (abstraction-grammar)
        (srfi :78))
;;;make-rule tests
(check (rule->define (make-rule 'F1 (make-options '((list 'a (list 'a (V25) (V25)))))))
       =>
       '(define (F1) (apply-option (lambda () (list 'a (list 'a (V25) (V25)))))))

(check (eval `(let ((V25 (lambda () (list 'b))))
                ,(rule->define (make-rule 'F1 (make-options '((list 'a (list 'a (V25) (V25))))))) (F1))
             (interaction-environment))
       =>
       '(a (a (b) (b))))

;;;make-start-rule tests
(let* ((F1 (lambda () (+ 1 1))))
  (check (rule->define (make-start-rule (make-options '((F1))))) => `(define (,START-SYMBOL) (apply-option (lambda () (F1))))))

(check (eval `(let ((F1 (lambda () (+ 1 1)))) ,(rule->define (make-start-rule (make-options '((F1))))) (,START-SYMBOL)) (interaction-environment)) => 2)

;;;make-grammar tests
(let* ([start-rule (make-start-rule (make-options '((F1))))]
       [rule-F1 (make-rule 'F1 (make-options '((list 'a (list 'a (V25) (V25))))))]
       [rule-V25 (make-rule 'V25 (make-options '((list 'b))))])
 (check (grammar->sexpr (make-grammar start-rule (list rule-F1 rule-V25)))
        =>
        `(let ()
           (define (,START-SYMBOL) (apply-option (lambda () (F1))))
           (define (F1) (apply-option (lambda () (list 'a (list 'a (V25) (V25))))))
           (define (V25) (apply-option (lambda () (list 'b))))
           (lambda () (,START-SYMBOL)))))

(let* ([start-rule (make-start-rule (make-options '((F1))))]
       [rule-F1 (make-rule 'F1 (make-options '((list 'a (list 'a (V25) (V25))))))]
       [rule-V25 (make-rule 'V25 (make-options '((list 'b))))]
       [grammar-sexpr (grammar->sexpr (make-grammar start-rule (list rule-F1 rule-V25)))])
     (check ((eval grammar-sexpr (interaction-environment))) => '(a (a (b) (b)))))

;;;abstraction->grammar tests



;;;multiple possible trees 
;; (let* ([start-rule (make-start-rule '((F1)))]
;;        [rule-F1 (make-function-rule 'F1 '(a (a (V25) (V25))))]
;;        [rule-V25 (make-variable-rule 'V25 '(b c d))]
;;        [grammar (make-grammar start-rule (rhs-options rule-F1 rule-V25))])
;;  (check-member ((eval grammar)) => '(a (a (b) (b)))))
(check-report)
(exit)