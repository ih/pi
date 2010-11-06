;;TO DO
;;-find out how to execute shell commands from scheme 
(import (church external py-pickle)
        (util)
        (church readable-scheme))

(define (depth tree)
  (if (list? tree)
      (+ 1 (apply max (map depth tree)))
      0))


(define draw-trees
  (py-pickle-script "./treedraw.py"))

(define animate
  (py-pickle-script "./animate.py"))

(define (animate-tree tree fname)
  (let* ([growth-sequence (tree-growth tree)])
    (draw-sequence fname growth-sequence)
    (animate fname)))

(define (draw-sequence fname tree-lst)
  (map (lambda (x y) (draw-trees (pair fname (list x)))) tree-lst (length tree-lst)))

(define (tree-growth tree)
  (let* ([max-depth (depth tree)]
         [all-depths (range 0 max-depth)])
    (map (lambda (x) (take-tree x tree)) all-depths))) 


(define (take-tree depth tree)
  (if (null? tree)
      '()
      (if (= depth 0)
          '()
          (append (list (first tree)) (delete '() (map (curry take-tree (- depth 1)) (rest tree)))))))

  
      

(animate "images/image")
    
    
    
