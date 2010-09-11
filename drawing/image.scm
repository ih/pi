(library (image)
         (export image test make-pixel make-image set-pixel! image-pixels get-pixel)
         (import (rnrs)
                 (srfi :25 multi-dimensional-arrays)
                 (srfi :25 multi-dimensional-arrays arlib))

         (define test 7)
         (define-record-type pixel (fields x y (mutable value)))

         (define-record-type image (fields width height (mutable pixels))
           (protocol
            (lambda (new)
              (lambda (width height)
                (new width height (tabulate-array (shape 0 width 0 height) (lambda (x y) (make-pixel x y 0))))))))

         (define (get-pixel image x y)
           (array-ref (image-pixels image) x y))

         (define (set-pixel! image x y value)
           (let ((target-pixel (get-pixel image x y)))
             (pixel-value-set! target-pixel value)))
         )
         ;; (define (set-pixel! image x y value)
         ;;   (array-set! (image-pixels image) x y value)))





         ;; (define-record-type point (fields x y distance)
         ;;   (protocol
         ;;    (lambda (new)
         ;;      (lambda (x y)
         ;;        (new x y (sqrt (+ (expt x 2) (expt y 2))))))))
