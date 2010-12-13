;;TO DO
;;-abstract common code for tree-replace-label and tree-insert
;;-generalize 3-color to n-color
(import (util)
        (church readable-scheme)
        (pi trees node))

;;;example thunk
  ;; (define concepts (list prototype single-recursion parameterized-parts))
  ;; ;;(define concepts (list multiple-recursion))
  ;; (define branch ((uniform-draw concepts)))
  ;; (define (trunk length label-generator)
  ;;   (if (= length 0)
  ;;       (apply node (list (label-generator) branch branch))
  ;;       (node (label-generator) (trunk (- length 1) label-generator))))
  ;; (define (all-a) 'a)
  ;; (define (random-label) (uniform-draw '(a b c d e f)))
  ;; (define label-generators (list all-a random-label))
  ;; (define lengths (range 1 10))
  ;; (trunk (uniform-draw lengths) (uniform-draw label-generators)))
(define (random-tree)
  (let ([size (random-integer max-random-tree-size)]
        [label-generator (random-label-generator)])
    (structure-of-size label-generator size)))
(define (random-label)
  (uniform-draw labels))

;;;constants
(define concepts (list random-tree))
(define max-random-tree-size 40)
(define label-generators (list random-label))
(define labels '(a b c d e f))
(define N 7)
;;;thunks
(define (gen-trunk-two-branches)
  (let* ([trunk (random-line)]
         [branch ((random-concept))])
    (trunk-two-branches branch trunk)))

(define (sizeNtree)
  (let ([label-generator (random-label-generator)])
    (structure-of-size label-generator N)))

(define (random-concept)
  (uniform-draw concepts))


(define (random-label-generator)
  (uniform-draw label-generators))


(define (random-line)
  (let ([size (random-from-range 1 max-random-tree-size)]
        [label-generator (random-label-generator)])
    (line size label-generator)))
  

;;;concepts
(define (trunk-two-branches branch trunk)
  (tree-insert branch (make-list (- (depth trunk) 1) 1) (tree-insert branch (make-list (- (depth trunk) 1) 1) trunk)))

(define (structure-of-size label-generator size)
  (if (= size 1)
      (list (label-generator))
      (let* ([num-branches (random-from-range 1 (- size 1))]
             [branch-sizes (create-partition num-branches (- size 1))])
        (pair (label-generator) (map (curry structure-of-size label-generator) branch-sizes)))))

(define (bottom-top-color bcolor tcolor bstructure tstructure connection-location)
  (tree-insert (color-tree tcolor tstructure) connection-location (color-tree bcolor bstructure)))

(define (same-shape scale tree)
  (define (attach-branches trunk branches)
    (let ([top (apply (curry node (first trunk)) branches)])
      (tree-insert top (make-list (- (length trunk) 1) 1) trunk)))
  (if (null? tree)
      '()
      (let ([scaled-branches (map (curry same-shape scale) (rest tree))]
            [scaled-trunk (line scale (lambda () (first tree)))])           (attach-branches scaled-trunk scaled-branches))))

(define (trunk-size trunk-length label-generator non-trunk-tree)
  (tree-insert non-trunk-tree (make-list (- trunk-length 1) 1) (line trunk-length label-generator)))

(define (three-color color tree)
  (let ([locations (repeat 3 (lambda () (random-location tree)))])
    (tree-replace-label color (third locations) (tree-replace-label color (second locations) (tree-replace-label color (first locations) tree)))))


;;;concept helper functions
(define (color-tree color tree)
  (sexp-replace 'x color tree))

(define (line length label-generator)
  (if (= length 0)
      '()
      (node (label-generator) (line (- length 1) label-generator))))

(define (create-partition number-partitions total)
  (if (= number-partitions 1)
      (list total)
      (let* ([partition-max-size (- total (- number-partitions 1))]
             [partition-size (+ (random-integer partition-max-size) 1)])
        (pair partition-size (create-partition (- number-partitions 1) (- total partition-size))))))


;;go all the way down to a leaf
;;shorten this path a random amount
(define (random-location tree)
  (let* ([initial-path (random-path-to-leaf tree)]
         [cut-off (random-from-range 0 (length initial-path))])
    (take initial-path cut-off)))

(define (random-path-to-leaf tree)
  (if (not (list? tree))
      '()
      (if (null? (rest tree))
          '()
          (let ([choice (random-from-range 1 (length (rest tree)))])
            (pair choice (random-path-to-leaf (list-ref tree choice)))))))


(define (tree-replace-label new-label location tree)
  (define (replace-label tree)
    (pair new-label (rest tree)))
  (tree-apply-proc replace-label location tree))


(define (tree-insert new-branch location tree)
  (define (insert-branch tree)
    (append tree (list new-branch)))
  (tree-apply-proc insert-branch location tree))



(define stim1btcolor (bottom-top-color 'g 'b '(x (x (x (x)))) '(x (x (x)) (x (x))) '(1 1 1)))
(define stim12branch1trunk (trunk-two-branches '(b (b (b))) '(g (g (g (g))))))


;;(pretty-print (repeat 5 gen-trunk-two-branches))
;; (pretty-print (tree-insert '(a (q)) '(1) '(a (b) (c))))
;; (pretty-print (tree-insert '(a) '() '(b)))
;; (pretty-print (tree-insert '(d) '(1 1) '(a (b (a (q))) (c))))

;; (pretty-print (create-partition 3 3))
;; (pretty-print (structure-of-size (lambda () 'x) 5))

;; (pretty-print (trunk-size 5 (lambda () 'g) (structure-of-size (lambda () 'x) 5)))


;; (pretty-print (node 'a (node 'b) (node 'c)))

;; (pretty-print (same-shape 2 '(a (b (c) (c)) (b))))

;; (pretty-print (tree-replace-label 'd '(1 1) '(a (b (a (q))) (c))))

;; (pretty-print (random-path-to-leaf '(a (b (c) (c)) (b))))

;; (pretty-print (three-color 'y '(a (b (c) (c)) (b))))
;;(equal? (tree-insert '(a) '(1) '(b)) '(b (a)))

;;(tree-insert '(a (b (b))) '(1) '(c (c (d) (d))))


;;(generate-static-stimulus line-recursion 3 "stimuli/line-recursion/tbd.png")