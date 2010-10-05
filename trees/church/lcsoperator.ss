(import (church))

(church
 ;do-common and do-uncommon may need to be passed seq1 and seq2 
 ;make-lcs-operator returns a memoized recursive function which does some variant of LCS
 (define (make-lcs-operator base-case do-common do-uncommon get-biggest)
   (define (lcs-operator seq1 seq2)
     (begin
       (define (item-match? row column)
         (equal? (list-ref seq1 row) (list-ref seq2 column)))

       (define (lcs-recursion row column)
         (if (or (= row -1) (= column -1))
             base-case
             (if (item-match? row column)
                 (let ((common-item (list-ref seq1 row))
                       (up-left-entry (lcs-recursion (- row 1) (- column 1))))
                   (do-common common-item up-left-entry))
                 (let ((left-entry (lcs-recursion (- row 1) column))
                       (up-entry (lcs-recursion row (- column 1))))
                   (do-uncommon (get-biggest left-entry up-entry)))
                 )))

       (define lcs-table (mem lcs-recursion))
       (lcs-table (- (length seq1) 1) (- (length seq2) 1))
       ))

   lcs-operator
     ;function tests
     ;(item-match? 1 2)
   )

 ;tests
 ;length computation
 (define length-base 0)
 (define (add1 common-item up-left-entry)
   (+ 1 up-left-entry))
 (define (length-get-biggest left-entry up-entry)
   (if (< left-entry up-entry)
       up-entry
       left-entry))
 (define (length-uncommon biggest)
   biggest)
 (define lcs-length (make-lcs-operator length-base add1 length-uncommon length-get-biggest))
 (lcs-length '(a b c b d a b a) '(b d c a b a))
 ;subsequences computation
 ;(define (lcs-base '(())))
; (define 
   
               
 
;;;item-match tests
 ;(equal? (lcs-op '(a b c) '(a b d)) false)
 ;(equal? (lcs-op '(a b) '(a b b)) true)
 )
(exit)
   

   
