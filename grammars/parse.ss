(import (church readable-scheme))
(define (sample distribution) (apply distribution '()))
(define (terminal t) (lambda () t))

(define EXPR
  (lambda ()
    (sample (uniform-draw (list IF BOOL)))))

(define IF
  (lambda ()
    (make-if (map sample (list EXPR EXPR EXPR)))))

(define BOOL
  (lambda ()
    (sample  (uniform-draw (list (terminal 't) (terminal 'f))))))


(define (parse expr) ())

(define (make-if args)
  (pair 'if args))



