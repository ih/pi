;;;horizontal lines all with the same y value, all same length
;;data examples
;;line of length 5
(set-pixel! i 3 4 1)
(set-pixel! i 4 4 1)
(set-pixel! i 5 4 1)
(set-pixel! i 6 4 1)
(set-pixel! i 7 4 1)
;;line of length 5
(set-pixel! i 15 4 1)
(set-pixel! i 14 4 1)
(set-pixel! i 13 4 1)
(set-pixel! i 12 4 1)
(set-pixel! i 11 4 1)
;;line of length 5 with pixels drawn out of order
(set-pixel! i 21 4 1)
(set-pixel! i 19 4 1)
(set-pixel! i 20 4 1)
(set-pixel! i 17 4 1)
(set-pixel! i 18 4 1)

;;human program
(define (hline-y4-len5 start-x)
  (define (draw-pixels x remaining)
    (if (= remaining 0)
      (set-pixel! i x 4 1)
      (begin 
        (set-pixel! x 4 1)
        (draw-pixels (+ x 1) (- remaining 1)))))
  (draw-pixels start-x 5))

;;abstraction
;;from a single line
(let () (define F1 (lambda (V1) (set-pixel! i V1 4 1)))
    (begin (F1 3) (F1 4) (F1 5) (F1 6) (F1 7)))
;;from mulitple lines
;;all pixels presented at once
(let () (define F1 (lambda (V1) (set-pixel! i V1 4 1)))
    (begin
      (F1 3)
      (F1 4)
      (F1 5)
      (F1 6)
      (F1 7)
      (F1 15)
      (F1 14)
      (F1 13)
      (F1 12)
      (F1 21)
      (F1 19)
      (F1 20)
      (F1 17)
      (F1 18)
      (F1 11))))

;;when the lines are separated with begin statements
(let () (define F1 (lambda (V1) (set-pixel! i V1 4 1)))
     (begin
       (begin (F1 3) (F1 4) (F1 5) (F1 6) (F1 7) (F1 15))
       (begin (F1 14) (F1 13) (F1 12) (F1 21))
       (begin (F1 19) (F1 20) (F1 17) (F1 18) (F1 11)))))


;;;we get a long program b/c there is no pattern found in the numbers, one possibility is to try and find patterns in the numbers by having a grammar for numbers and using abstraction

;;from multiple abstracted lines





(define max-y 28)
(define max-x 28)
(define (same-y)
  (let* ([start-x (sample-integer max-x)]
         [line-length (sample-integer (- max-x start-x))])
    (draw-line 