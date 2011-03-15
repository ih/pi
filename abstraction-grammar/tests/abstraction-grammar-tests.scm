(import (rnrs)
        (abstract)
        (abstraction-grammar)
        (srfi :78))
;;;make-start-rule test
(let* ((F1 (lambda () (+ 1 1))))
  (check (make-start-rule (rhs-options '(F1))) => `(define (,START-SYMBOL) (apply-option (lambda () (F1))))))

(check (eval `(let ((F1 (lambda () (+ 1 1)))) ,(make-start-rule (rhs-options '(F1))) (,START-SYMBOL)) (interaction-environment)) => 2)

;;;make-function-rule
(check (make-rule 'F1 (rhs-options '`(a (a (,(V25)) (,(V25)))))) => '(define (F1) (apply-option (lambda () `(a (a (,(V25)) (,(V25))))))))

(check (eval `(let ((V25 (lambda () 'b))) ,(make-rule 'F1 (rhs-options '`(a (a (,(V25)) (,(V25)))))) (F1)) (interaction-environment)) => '(a (a (b) (b))))
;;;make-grammar tests
(let* ([start-rule (make-start-rule (rhs-options '(F1)))]
       [rule-F1 (make-rule 'F1 (rhs-options '`(a (a (,(V25)) (,(V25))))))]
       [rule-V25 (make-rule 'V25 (rhs-options ''b))])
 (check (make-grammar (list start-rule rule-F1 rule-V25))
        =>
        `(let ()
           (define (,START-SYMBOL) (apply-option (lambda () (F1))))
           (define (F1) (apply-option (lambda () `(a (a (,(V25)) (,(V25)))))))
           (define (V25) (apply-option (lambda () 'b)))
           (lambda () (,START-SYMBOL)))))
(let* ([start-rule (make-start-rule (rhs-options '(F1)))]
       [rule-F1 (make-rule 'F1 (rhs-options '`(a (a (,(V25)) (,(V25))))))]
       [rule-V25 (make-rule 'V25 (rhs-options ''b))]
       [grammar (make-grammar (list start-rule rule-F1 rule-V25))])
  (check ((eval grammar (interaction-environment))) => '(a (a (b) (b)))))
;;;multiple possible trees 
;; (let* ([start-rule (make-start-rule '((F1)))]
;;        [rule-F1 (make-function-rule 'F1 '(a (a (V25) (V25))))]
;;        [rule-V25 (make-variable-rule 'V25 '(b c d))]
;;        [grammar (make-grammar start-rule (rhs-options rule-F1 rule-V25))])
;;  (check-member ((eval grammar)) => '(a (a (b) (b)))))
(check-report)
(exit)