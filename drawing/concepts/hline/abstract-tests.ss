(import (pi abstract))

;;three lines
(define test-data '(let () (begin (begin (set-pixel! i 3 4 1) (set-pixel! i 4 4 1) (set-pixel! i 5 4 1) (set-pixel! i 6 4 1) (set-pixel! i 7 4 1) (set-pixel! i 15 4 1)) (begin (set-pixel! i 14 4 1) (set-pixel! i 13 4 1) (set-pixel! i 12 4 1) (set-pixel! i 21 4 1)) (begin (set-pixel! i 19 4 1) (set-pixel! i 20 4 1) (set-pixel! i 17 4 1) (set-pixel! i 18 4 1) (set-pixel! i 11 4 1)))))

;;three abstract lines
;;(define test-data '(let () (begin (let () (define F1 (lambda (V1) (set-pixel! i V1 4 1))) (begin (F1 3) (F1 4) (F1 5) (F1 6) (F1 7))) (let () (define F1 (lambda (V1) (set-pixel! i V1 4 1))) (begin (F1 15) (F1 14) (F1 13) (F1 12) (F1 11))) (let () (define F1 (lambda (V1) (set-pixel! i V1 4 1))) (begin (F1 21) (F1 19) (F1 20) (F1 17) (F1 18))))))
                                      

(define program-form (sexpr->program test-data))

(pretty-print test-data)
(pretty-print (map program->sexpr (all-iterated-compressions program-form)))
(exit)