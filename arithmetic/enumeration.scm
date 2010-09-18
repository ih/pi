(library (enumeration)
         (export all-trees)
         (import (rnrs)
                 (only (srfi :1 lists) first second lset-difference))

         (define rest cdr)
         (define operators '(+ *))

         (define (all-trees items)
           (if (<= (length items) 2)
               (make-pairs (first items) (second items))
               (let* ((pairs (choose-pairs items))
                      (new-items (make-new-items pairs items)))
                 (apply append (map all-trees new-items)))))

         (define (choose-pairs items)
           (if (<= (length items) 2)
               (list items)
               (append (pair-with (first items) (rest items)) (choose-pairs (rest items)))))

         (define (pair-with item others)
           (if (= (length others) 1)
               (make-pairs item (first others))
               (append (make-pairs item (first others)) (pair-with item (rest others)))))

         (define (make-pairs a b)
           (let ((add-operator (add-operator-function a b)))
             (map add-operator operators)))
         
         (define (add-operator-function a b)
           (lambda (operator)
             (list operator a b)))
         
         (define (make-new-items pairs items)
           (let ((make-new-item (make-new-item-function items)))
             (map make-new-item pairs)))

         (define (make-new-item-function items)
           (lambda (pair)
             (cons pair (lset-difference eq? items pair))))
         )










