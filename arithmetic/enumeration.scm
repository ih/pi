(define rest cdr)
(import (srfi :1 lists))
(define (all-trees items)
  (if (<= (length items) 2)
      (list items)
      (let* ((pairs (choose-pairs items))
             (new-items (make-new-items pairs items)))
        (apply append (map all-trees new-items)))))

(define (choose-pairs items)
  (if (<= (length items) 2)
      (list items)
      (append (pair-with (first items) (rest items)) (choose-pairs (rest items)))))

(define (pair-with item others)
  (if (= (length others) 1)
      (list (cons item others))
      (cons (list item (first others)) (pair-with item (rest others)))))

(define (make-new-items pairs items)
  (let ((make-new-item (make-new-item-function items)))
    (map make-new-item pairs)))

(define (make-new-item-function items)
  (lambda (pair)
    (cons pair (lset-difference eq? items pair))))




(all-trees '(a b))





