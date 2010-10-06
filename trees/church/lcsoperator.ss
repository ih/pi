(import (church))

(church
 ;;;general functions; these should be imported somehow
   (define (member? item lst)
    (if (null? lst)
        false
        (if (equal? item (first lst))
            true
            (member? item (rest lst)))))

  (define (curry fun . args)
   (lambda x
     (apply fun (append args x))))
  (define (id x) x)

  (define (gen-sym)
    (second (gensym)))

  (define (get-biggest get-size combine x y)
    (let ((size-x (get-size x))
          (size-y (get-size y)))
      (cond ((< size-x size-y) y)
            ((> size-x size-y) x)
            (else (combine x y)))))

  (define (set-append lst1 lst2)
    (delete-duplicates (append lst1 lst2)))
  
  (define (delete-duplicates lst)
    (delete-duplicates-helper '() lst))

  (define (delete-duplicates-helper set lst)
    (if (null? lst)
        set
        (delete-duplicates-helper
         (if (member? (first lst) set)
             set
             (pair (first lst) set))
         (rest lst))))
          
          
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
   (get-biggest id (lambda (x y) x) left-entry up-entry))

 (define (length-uncommon biggest)
   biggest)
 (define lcs-length (make-lcs-operator length-base add1 length-uncommon length-get-biggest))

 ;subsequences computation
 ;eventually import records into church and define a record for the entry
 (define (make-lcs-entry variable-info expressions)
   (pair variable-info expressions))
 (define get-entry-expressions second)
 (define get-entry-variable-info first)
 (define get-entry-variables first)
 
 (define lcs-base '(()()))
 (define (lcs-common common-item up-left-entry)
   (let* ((old-expressions (get-entry-expressions up-left-entry))
          (new-expressions (add-item common-item old-expressions))
          (old-variable-info (get-entry-variable-info up-left-entry)))
     (make-lcs-entry old-variable-info new-expressions)))
 (define (add-item common-item expressions)
   (map (curry pair common-item) expressions))
 
 (define (lcs-get-biggest left-entry up-entry)
   (get-biggest entry-subsequence-length join-entries left-entry up-entry))

 ;set-append variables and expression lists
 (define (join-entries entry1 entry2)
   (let* ((expressions1 (get-entry-expressions entry1))
          (expressions2 (get-entry-expressions entry2))
          (variable-info1 (get-entry-variable-info entry1))
          (variable-info2 (get-entry-variable-info entry2))
          (new-variable-info (set-append variable-info1 variable-info2))
          (new-expressions (set-append expressions1 expressions2)))
     (make-lcs-entry new-variable-info new-expressions)))

 ;length of the expression without variables
 (define (entry-subsequence-length entry)
   (length (get-subsequence entry)))

 (define (get-subsequence entry)
   (let ((variables (get-variables entry))
         (expression (first (get-expressions entry))))
     (strip-variables variables expressions)))

 (define (strip-variables variables expression)
   (let ((not-variable (make-not-variable-filter variables)))
     (filter not-variable expression)))

 (define (make-not-variable-filter variables)
   (lambda (x)
     (not (member? x variables))))
 
 (define (lcs-uncommon biggest-entry)
   (add-variable biggest-entry))
;you have to create the variable and add it to a subsequence at the same time and this should be memoized, the variable also has to be added to variables (maybe set-appended)
 (define (add-variable entry)
   (let ((variables (get-variables entry))
         (expressions (get-expressions entry)))
     (make-lcs-entry (add-to-variables variables

                                       
 (define append-variable
   (mem (lambda (subsequence)
          (if (null? subsequence)
              subsequence
              (if (in-a-sequence? (first subsequence))
                  (pair (gen-sym) subsequence)
                  subsequence)))))

 
      (define (add-variables entry)
        (map add-variable entry))
      (define (in-a-sequence? item)
        (or (member? item seq1) (member? item seq2)))

      ))

 (define lcs (make-lcs-operator lcs-base lcs-common lcs-uncommon lcs-get-biggest))




 ;eventually implement this in terms of filter
 ;; (define (strip-variables expression)
 ;;   (if (null? expression)
 ;;       '()
 ;;       (let ((item (first expression)))
 ;;         (if (list? item)
 ;;             (strip-variables item)
 ;;             (if (in-a-sequence? item)
 ;;                 (pair exp (strip-variables (rest expression)))
 ;;                 (strip-variables (rest expression)))))))


 
; (define 
   
               
 
;;;item-match tests
 ;(equal? (lcs-op '(a b c) '(a b d)) false)
 ;(equal? (lcs-op '(a b) '(a b b)) true)
;;;curry test
 ;((curry (lambda (x y) (+ x y)) 3) 4)
;;;lcs tests
 ;(lcs-common 'a '((b b c) (b d e)))
 ;(lcs-length '(a b c b d a b a) '(b d c a b a))
; (lcs '(a b c b d a b a) '(b d c a b a))
; (first '(()))
 )
(exit)
   

   
