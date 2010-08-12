(import (srfi :25 multi-dimensional-arrays))
(import (srfi :25 multi-dimensional-arrays arlib))

(define-record-type point (fields x y distance)
  (protocol
   (lambda (new)
     (lambda (x y)
       (new x y (sqrt (+ (expt x 2) (expt y 2))))))))

(define testPoint (make-point 3 4))

(define-record-type image (fields width height (mutable pixels))
  (protocol
   (lambda (new)
     (lambda (width height)
       (new width height (tabulate-array (shape 0 10 0 10) (lambda (x y) (make-point x y))))))))

(define (set-pixel! image pixel value)
  (array-set! (image-pixels image) (point-x pixel) (point-y pixel) value))

(define testImage (make-image 10 10))
(define testImage (make-array (shape 0 10 0 10)))



(imageSet! point image)

(array-set! testImage 2 2 1)

(array-map (image-pixels testImage) (shape 0 10 0 10) (lambda (x) x))

;loop through each pixel and output set-pixel! if a pixel is set to value 1
;the order of visitation seems to matter as far as abstraction goes
(define (deval data)
  (if (number? data)
      '(set-pixel? 
  (mapPixels deval image)