;;TO DO
;;- write rule to generate if expressions
;;- write rule to generate part

(import (_srfi :1)
        (church)
        (church readable-scheme))

(define (sample distribution) (apply distribution '()))

(define (terminal t) (lambda () t))

(define LABEL
  (lambda ()
    (sample 
    (uniform-draw
     (list
      (terminal 'a)
      (terminal 'b)
      (terminal 'c))))))


;; (define TREE-EXPR
;;   (lambda ()
;;     (map sample (uniform-draw (list (list IF) (list NODE) (list FUNC-APP) (list (terminal '())))))))

(define TREE-EXPR
  (lambda ()
    (sample (uniform-draw (list IF NODE FUNC-APP (terminal '()))))))


(define FUNC-APP
  (lambda ()
    (make-application (uniform-draw functions))))

(define IF
  (lambda ()
    (make-if (map sample (list TREE-EXPR TREE-EXPR)))))


(define NODE
  (lambda ()
    (make-node (map sample (list LABEL TREE-EXPR)))))

(define functions (list (list 'F1 (list LABEL TREE-EXPR)) (list 'F2 '()) (list 'F3 (list LABEL LABEL))))

(define (make-application func-info)
  (let ([name (first func-info)]
        [args (second func-info)])
    (pair name (map sample args))))
    
;;
(define (make-tree-expr a) (first a))

(define (make-if if-info)
  (let ([true-condition (first if-info)]
        [false-condition (second if-info)])
    `(if (flip) ,true-condition ,false-condition)))


(define (make-node node-info)
  (let* ([label (first node-info)]
        [expr (second node-info)])
;;        [db (pretty-print (list "expr" expr))])
    (if (null? expr)
        (list 'node label)
        (list 'node label expr))))



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
