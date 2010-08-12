(import (srfi :1 lists))

(define-record-type point (fields x y distance)
  (protocol
   (lambda (new)
     (lambda (x y)
       (new x y (sqrt (+ (expt x 2) (expt y 2))))))))

;top-down try to fit models and knock out blocks of data; recording models and subsets of the data they explain
(define (fit-data-models possible-models unexplained-data explanation)
  (if (all-data-explained? unexplained-data)
      explanation
      (let* ((current-model (choose-model possible-models))
             (possible-explanation (explain-data current-model unexplained-data))
             (if (good-enough? possible-explanation)
                 (fit-data-models data-models (remove-explained possible-explanation unexplained-data) (add-to-explanation possible-explanation explanation))
                 (fit-data-models (remove-model current-model possible-models) unexplained-data explanation))))))


                 (if 
                 (begin
                   (model-iterator 'reset)
                   (data-iterator 
                   (fit-data-models data-models (remove-data current-data unexplained-data) (add-data current-data explained-data))
                 (fit-data-models data-models (remove-data current-data unexplained-data) (add-data current-data explained-data))
                 
(define (explain-data-with-model hypothesis unexplained-data explained-data)
  (if (explained? unexplained-data)
      (add-data explained-data hypothesis)


(define (fitness expr data)
  (cond ((draw-pixel? expr)
         (if (coordinate-match? expr data)
             1
             0))
        (else 0)))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      false))

(define (draw-pixel? expr)
  (tagged-list? expr 'draw-pixel))

(define (coordinate-match? expr pixel)
  (and (equal? (pixel-expr-x expr) (point-x pixel))
       (equal? (pixel-expr-y expr) (point-y pixel))))

(define (pixel-expr-x expr)
  (second expr))

(define (pixel-expr-y expr)
  (third expr))



(define (program-induce data object-library connector-library)
  (let ((disjoint-models (fit-object-models data object-library))
        (composite-model (connect-models disjoint-models connector-library)))
    (program-induce data (abstract-objects object-library composite-model) (abstract-connectors connector-library composite-model))))

(define (init-object-model '(set-pixel! image pixel value)))
(define (init-connector-model ))
(define (fit-object-models
         (order-models! data object-library)
         (map (fit-object-model data) object-library)))

(define (eval ast)
  (cond (equal? (first ast) 'set-pixel!)
        (eval ast)))
(define (fit-object-model data model)
  (order-data! data model))

(define (program-induction data-models process-models))
  
(define (model-selector)
  (last-model ))
(define (model-data data)
  (let (select-model data init-data-models)))

      
(define (fitness model data)
  (cond ((is-draw-pixel? model)
         (if (and
              (equal (point-x (pixel model)) (point-x data))
              (equal (point-y (pixel model)) (point-y data)))
             1
             0)
         (else 0))))
         
        
         
        
                         