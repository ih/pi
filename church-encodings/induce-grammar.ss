;;TODO:
;; - write a parse function for computations with and without input


(import (abstract))
(import (parse))


(define (learn grammar data rewards)
  (let ([parses (parse-all data)]
        [abstractions (abstract parses)])
    (if (satisfactory

(define zero '(lambda (f) (lambda (zero) zero)))
(define one '(lambda (f) (lambda (zero) (f zero))))
(define two '(lambda (f) (lambda (zero) (f (f zero)))))
(define three '(lambda (f) (lambda (zero) (f (f (f zero))))))
(define four '(lambda (f) (lambda (zero) (f (f (f (f zero)))))))
(define five '(lambda (f) (lambda (zero) (f (f (f (f (f zero))))))))
(define six '(lambda (f) (lambda (zero) (f (f (f (f (f (f zero)))))))))

(define data '((< a b) (< a c) (< a d) (< a e) (< a f) (< a g)
               (< b c) (< b d) (< b e) (< b f) (< b g) 
               (< c d) (< c e) (< c f) (< c g)
               (< d e) (< d f) (< d g)
               (< e f) (< e g)
               (< f g)))



;(define data (list one two three four five six))

;(test-compression '((a b c) (a b c) (a b c)))
;(test-compression (list one two three four))
(test-compression data)


;;TASK:
;; - write something like test-compression and run it on a set of church numerals

