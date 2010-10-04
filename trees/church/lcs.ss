(import (srfi :1 lists))
(import (church))
(church
 (define (list-ref lst index)
   (if (= 0 index)
       (first lst)
       (list-ref (rest lst) (- index 1))))

(define (lcs seq1 seq2)
  (begin
    (define (item-match? row column)
      (equal? (list-ref seq1 row) (list-ref seq2 column)))

    (define (length-table-recursion row column)
      (if (or (= row -1) (= column -1))
          0
          (if (item-match? row column)
              (let* ((up-left (length-table-recursion (- row 1) (- column 1))))
                (+ up-left 1))
              (let* ((left (length-table-recursion (- row 1) column))
                     (up (length-table-recursion row (- column 1))))
                (max left up)))))

    (define length-table (mem length-table-recursion))

    (define (recover-lcs row column)
      (let ((length (length-table row column)))
        (if (= length 0)
            '()
            (if (item-match? row column)
                (let ((common-character (list-ref seq1 row)))
                  (pair common-character (recover-lcs (- row 1) (- column 1))))
                (let ((next-coordinates (determine-next-move row column)))
                  (recover-lcs (get-x next-coordinates) (get-y next-coordinates)))))))

    (define (determine-next-move row column)
      (let ((up-length (length-table row (- column 1)))
            (left-length (length-table (- row 1) column)))
        (if (< up-length left-length)
            (make-coordinate (- row 1) column)
            (make-coordinate row (- column 1)))))

    (length-table (- (length seq1) 1) (- (length seq2) 1))
    (recover-lcs (- (length seq1) 1) (- (length seq2) 1))
   ))

(define (make-coordinate i j)
  (list i j))
(define get-x first)
(define get-y second)

;(lcs '(x f x a x b c y d y y) '(x g x a v d y y))



;;;all subsequences...
(define (cs seq1 seq2)
  (begin
  (define (item-match? row column)
    (equal? (list-ref seq1 row) (list-ref seq2 column)))

  (define (cs-recursion row column)
    (if (or (= row -1) (= column -1))
        ;(list (list (gen-sym)))
        '(())
        (if (item-match? row column)
            (let* ((common-item (list-ref seq1 row))
                   (up-left-sequences (cs-recursion (- row 1) (- column 1))))
              (make-sequences common-item up-left-sequences))
            (let* ((left-sequences (cs-recursion (- row 1) column))
                   (up-sequences (cs-recursion row (- column 1))))
              (add-variables (get-longer-sequences left-sequences up-sequences))))))
              ;(get-longer-sequences left-sequences up-sequences)))))
  (define (make-sequences item seqs)
    (map (lambda (y) (pair item y)) seqs)) 
  (define (get-longer-sequences seqs1 seqs2)
    (let ((length1 (length (first seqs1)))
          (length2 (length (first seqs2))))
      (cond ((> length1 length2) seqs1)
            ((< length1 length2) seqs2)
            (else (set-append seqs1 seqs2)))))

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

  
  (define (add-variables subsequences)
    (map add-variable subsequences))
  (define (add-variable subsequence)
    (if (null? subsequence)
        subsequence
        (if (in-a-sequence? (first subsequence))
            (pair (gen-sym) subsequence)
            subsequence)))
  (define (in-a-sequence? item)
    (or (member? item seq1) (member? item seq2)))
  (define (member? item lst)
    (if (null? lst)
        false
        (if (equal? item (first lst))
            true
            (member? item (rest lst)))))
  (define (gen-sym)
    (second (gensym)))

                        
  
  (define cs-table (mem cs-recursion))  
  
  (define (row-iter i j)
    (if (and (= i 0) (= j 0))
        (cs-table i j)
        (if (= i 0)
            (set-append (cs-table i j) (row-iter (- (length seq1) 1) (- j 1)))
            (set-append (cs-table i j) (row-iter (- i 1) j)))))

  (cs-table (- (length seq1) 1) (- (length seq2) 1))
        
;  (row-iter (- (length seq1) 1) (- (length seq2) 1))
  )


  )

;(cs (
;(cs '(a b c b d a b) '(b d c a b a))
;(cs '(x f x a x b c y d y y) '(x g x a v d y y))
(cs '(a a a) '(a b b a))
;; (define (common-subsequences seq1 seq2)
;;   (let ((subsequences '())
;;   (begin
    
;;   (define (cs-recursion i j)
;;     (if (or (= i -1) (= j -1))
;;         (add-subsequence '())
;;         (if (equal? (list-ref seq1 i) (list-ref seq2 j))
;;             (let* ((common-character (list-ref seq1 i))
;;                    (up-left (cs-recursion (- i 1) (- j 1) (extend-subsequence common-character subsequences i j))))
;;               (+ up-left 1))
;;             (let* ((left (cs-recursion (- i 1) j '()))
;;                    (up (cs-recursion i (- j 1) '())))
;;               (max left up)))))
;;   ((mem cs-recursion) (- (length seq1) 1) (- (length seq2) 1) '())
;;   subsequences
;;   ))

;; (define (extend-subsequence common-character subsequences i j)
;;   (let ((target-subsequences (find-subsequences i j subsequences))
;;         (new-subsequences (map (curry add-character common-character) target-subsequences)))
;;     (add-subsequences new-subsequences subsequences)))

;; (define (find-subsequences i j subsequences)
;;   (filter (curry matching-coordinate? i j) subsequences))

;; (define (matching-coordinate? x y subsequence)
;;   (or (= (last-item-x subsequence) x) (= (last-item-y subsequence) y)))
        
    
;; (common-subsequences '(a b c b d a b a) '(b d c a b a))
;; ;(length '(a b c b d a b))
 
 
)
(exit)
                         
