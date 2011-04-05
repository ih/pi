;;;this is wrong, having the variables as thunks makes everything context free, you have to have the rhs of the non-terminals be functions that take arguments or somehow have a variable be generated only once e.g (lambda () (let ([V25 (V25)]) (list 'a (list 'a V25 V25))))

(import (rnrs)
        (abstract)
        (abstraction-grammar)
        (only (church readable-scheme) repeat)
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

;;;abstraction program->grammar tests
;;create-rule-applications
(let* ([sexprs '((F8 'b) (F8 'c) (F8 'd))])
   (check (create-rule-applications sexprs) => '((F8) (F8) (F8))))
;;body->options
(let* ([body '(uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))])
  (check (body->options body) => '((lambda () (F8)) (lambda () (F8)) (lambda () (F8)))))
;;program->start-rule
(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))))])
  (check (rule->define (program->start-rule program)) => `(define (,START-SYMBOL) (apply-option (lambda () (F8)) (lambda () (F8)) (lambda () (F8))))))
;;;
;;variable->rule
(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))))]
       [abstraction (define->abstraction '(define F8
                                            (lambda (V25) (list 'a (list 'a (list V25) (list V25))))))])
  (check (rule->define (variable->rule program abstraction 'V25))
         =>
         '(define (V25) (apply-option (lambda () 'b) (lambda () 'c) (lambda () 'd)))))
;;abstraction->rules
(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))))]
       [abstraction (define->abstraction '(define F8
                                            (lambda (V25) (list 'a (list 'a (list V25) (list V25))))))])
  (check (map rule->define (abstraction->rules program abstraction))
         =>
         '((define (F8) (apply-option (lambda () (list 'a (list 'a (list (V25)) (list (V25)))))))
           (define (V25) (apply-option (lambda () 'b) (lambda () 'c) (lambda () 'd))))))

;;program->rules
(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))))])
  (check (map rule->define (program->rules program))
         =>
         '((define (F8) (apply-option (lambda () (list 'a (list 'a (list (V25)) (list (V25)))))))
           (define (V25) (apply-option (lambda () 'b) (lambda () 'c) (lambda () 'd))))))
;; ;;program->grammar
(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b) (F8 'c) (F8 'd)))))])
  (check (grammar->sexpr (program->grammar program))
         =>
         `(let ()
           (define (,START-SYMBOL) (apply-option (lambda () (F8)) (lambda () (F8)) (lambda () (F8))))
           (define (F8) (apply-option (lambda () (list 'a (list 'a (list (V25)) (list (V25)))))))
           (define (V25) (apply-option (lambda () 'b) (lambda () 'c) (lambda () 'd)))
           (lambda () (,START-SYMBOL)))))

(let* ([program (sexpr->program '(let ()
                                   (define F8
                                     (lambda (V25) (list 'a (list 'a (list V25) (list V25)))))
                                   (uniform-draw (list (F8 'b)))))])
  (check ((eval (grammar->sexpr (program->grammar program)) (interaction-environment)))
         =>
         '(a (a (b) (b)))))

(let* ([program (sexpr->program '(let () (define F1 (lambda (V2) (list 'a V2)))
                                      (uniform-draw
                                       (list (F1 (list 'a)) (F1 (F1 (F1 (list 'a))))
                                             (F1 (F1 (list 'a))) (list 'a)
                                             (F1 (F1 (F1 (F1 (F1 (list 'a))))))))))])
  (check (grammar->sexpr (program->grammar program))
         =>
         `(let ()
            (define (,START-SYMBOL) (apply-option (lambda () (F1)) (lambda () (F1)) (lambda () (F1)) (lambda () (list 'a)) (lambda () (F1))))
            (define (F1) (apply-option (lambda () (list 'a (V2)))))
            (define (V2) (apply-option (lambda () (list 'a)) (lambda () (F1)) (lambda () (F1)) (lambda () (list 'a)) (lambda () (F1)) (lambda () (list 'a)) (lambda () (F1)) (lambda () (F1)) (lambda () (F1)) (lambda () (F1)) (lambda () (list 'a))))
            (lambda () (,START-SYMBOL)))))


;; ;;;multiple possible trees 
;; ;; (let* ([start-rule (make-start-rule '((F1)))]
;; ;;        [rule-F1 (make-function-rule 'F1 '(a (a (V25) (V25))))]
;; ;;        [rule-V25 (make-variable-rule 'V25 '(b c d))]
;; ;;        [grammar (make-grammar start-rule (rhs-options rule-F1 rule-V25))])
;; ;;  (check-member ((eval grammar)) => '(a (a (b) (b)))))
(check-report)

(let* ([program (sexpr->program '(let () (define F1 (lambda (V2) (list 'a V2)))
                                      (uniform-draw
                                       (list (F1 (list 'a)) (F1 (F1 (F1 (list 'a))))
                                             (F1 (F1 (list 'a))) (list 'a)
                                             (F1 (F1 (F1 (F1 (F1 (list 'a))))))))))])
  (repeat 20 (eval (grammar->sexpr (program->grammar program)) (interaction-environment))))