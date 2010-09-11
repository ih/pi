(import (image))

(define testImage (make-image 20 10))

(get-pixel testImage 3 4)

(set-pixel! testImage 3 4 1)

(get-pixel testImage 3 4)

testImage

(define testPixel (make-pixel 3 4))


(imageSet! point image)

(set-pixel! testImage testPoint 1)



(array-map (image-pixels testImage) (shape 0 10 0 10) (lambda (x) x)))

