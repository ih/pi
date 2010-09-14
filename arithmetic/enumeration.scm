(define rest cdr)
(import (srfi :1 lists))
(define (all-trees items)
  (if (<= (length items) 2)
      (list items)
      (let* ((pairs (choose-pairs items))
             (new-items (make-new-items pairs items)))
        (map all-trees new-items))))

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











(define all-items '(a b c d))

(define (pairs-with item others)
  (let ((items (cons item others))
  (define (pairs-with-recursion 
  (let* ((pair (list item (first others)))
         (non-pair-items (set-minus items pair)))
    (cons (cons pair non-pair-items) (pairs-with item (rest others)))))
(pairs-with (first items) (rest items))

  
  (define (pairs-with item others)
    (let* ((pair (list item (first others)))
          (non-pair-items (set-minus items pair)))
      (cons (cons pair non-pair-items) (pairs-with item (rest others)))))


(define (choose-pairs items)
  (let* ((head (first items))
         (pairs (pairs-with head (rest items)))
         (new-item-list (combined-items) 
                         
    
    (append  (choose-pairs (rest items)))))

    (cons 
  (define (make-pairs-with all-items)
    (define  (item others)
      (let ((pair (list item (first others))))
        (cons (append  pair (set-minus all-items pair)) (




