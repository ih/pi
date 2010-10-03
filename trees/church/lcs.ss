(import (srfi :1 lists))
(import (church))
(church
 (define (list-ref lst index)
   (if (= 0 index)
       (first lst)
       (list-ref (rest lst) (- index 1))))
 (define (lcs-recursion seq1 seq2 i j)
   (if (or (= i -1) (= j -1))
       0
       (if (equal? (list-ref seq1 i) (list-ref seq2 j))
           (let* ((a (lcs-recursion seq1 seq2 (- i 1) (- j 1)))
                  (db (pretty-print (list "eq" (+ a 1) i j))))
             (+ a 1))
           (let* ((b (lcs-recursion seq1 seq2 (- i 1) j))
                  (c (lcs-recursion seq1 seq2 i (- j 1)))
                  (db (pretty-print (list "neq" b c i j))))
             (max b c)))))
 (define lcs (mem lcs-recursion))
 ;(list-ref '(a b c) 2)
; (lcs '(a b c b d a b) '(b d c a b a) 6 5)
(define s1 '(a b c b d a b))
(define s2 '(b d c a b a b))
;;;try scanning the already created "table"
 (define (recover-LCS seq1 seq2 i j)
   (let ((length (lcs seq1 seq2 i j)))
     (if (= length 0)
         '()
         (if (item-match? seq1 seq2 i j)
             (let ((common-character (list-ref seq1 i)))
               (pair common-character (recover-LCS seq1 seq2 (- i 1) (- j 1))))
             (let ((next-coordinates (determine-next-move seq1 seq2 i j)))
               (recover-LCS seq1 seq2 (get-x next-coordinates) (get-y next-coordinates)))))))

 (define (determine-next-move seq1 seq2 i j)
   (let ((up-length (lcs seq1 seq2 i (- j 1)))
         (left-length (lcs seq1 seq2 (- i 1) j)))
     (if (< up-length left-length)
         (make-coordinate (- i 1) j)
         (make-coordinate i (- j 1)))))

(define (make-coordinate i j)
  (list i j))
(define get-x first)
(define get-y second)
  
 (define (item-match? seq1 seq2 i j)
   (equal? (list-ref seq1 i) (list-ref seq2 j)))
       
 (lcs s2 s1 6 6)

(determine-next-move s2 s1 5 5)


(recover-LCS s2 s1 6 6)
;;;all subsequences...
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
                         
