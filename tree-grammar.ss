;;TO DO
;;- write rule to generate if expressions
;;- write rule to generate part

(import (_srfi :1)
        (church)
        (church readable-scheme))

(define (sample distribution) (apply distribution '()))

(define (terminal t) (lambda () t))

;; (define LABEL
;;   (lambda ()
;;     (map sample
;;          (uniform-draw
;;           (list 
;;            (list (terminal 'a))
;;            (list (terminal 'b))
;;            (list (terminal 'c)))))))


;;(RULE 'TREE-EXPR make-tree '((IF) (NODE)))

(define LABEL
  (lambda ()
    (sample 
    (uniform-draw
     (list
      (terminal 'a)
      (terminal 'b)
      (terminal 'c))))))


(define TREE-EXPR
  (lambda ()
    (map sample (uniform-draw (list (list IF) (list NODE) (list (terminal '())))))))

(define IF
  (lambda ()
    (make-if (map sample (list TREE-EXPR TREE-EXPR)))))


(define NODE
  (lambda ()
    (make-node (map sample (list LABEL TREE-EXPR)))))


(define (make-tree-expr a) (first a))

(define (make-if if-info)
  (let ([true-condition (first (first if-info))]
        [false-condition (first (second if-info))])
    `(if (flip) ,true-condition ,false-condition)))


(define (make-node node-info)
  (let ([label (first node-info)]
        [expr (first (second node-info))])
    (list 'node label expr)))



(define node-info->label first)
;; (define NODE-LIST
;;   (lambda ()
;;     (map sample
;;          (uniform-draw
;;           (list (terminal '()) 


                
;;(define 
;; (define LABEL
;;   (lambda ()
;;     (uniform-draw '(a b c d))))

;; (define TREE-EXPR
;;   (lambda ()
;;     (multinomial (list (IF) (list 'node (LABEL))) (list .1 .9))))

;; (define IF
;;   (lambda ()
;;     (list 'if (flip) (TREE-EXPR) (TREE-EXPR))))

;; (TREE-EXPR)





;;(LABEL)
    
    ;; (define P
;;   (lambda ()
;;     `(let () ,(LA) ,(B))))

;; (define LA
;;   (lambda ()))

;; (define TREE-EXPR
;;   (lambda ()
;;     (map sample
;;   `(node ,LABEL TREE-EXPR))))

;; 
