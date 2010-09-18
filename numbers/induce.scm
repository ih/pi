(define two (1 1))
(define four (1 1 1 1))
(define six (1 1 1 1 1 1))
(define eight (1 1 1 1 1 1 1 1))

(define primitives '(1 0))
(define operators '(+))

(define (induce objects types time)
  (while (time-not-up time)
    (let* ((computations (map generate-trees types objects))
           (possible-patterns (find-common-subtrees computations))
           (new-types (create-types possible-patterns)))
      (add-set! types new-types))))
         

;;;based on the existing operators and primitives generate trees that evaluate to the object; this might eventually be put into a parallel processing system and so it would continuously run and be constantly outputing possible trees
(define (generate-trees object time)
  (let ((valid-trees '()))
    (while (not (times-up time))
      (set-add! (generate-tree (select-one types) object) valid-trees))
    valid-trees))

;;;generate tree keeps track of previous trees to try and ensure new trees are generated each time, this could also become a ranomized procedure for simplicity
;;;each operator should have a sampler defined for each of its children this corresponds to the child/argument's type
;;;each type or sampler for the type has a (prior) distribution over the patterns that define the type and these patterns/abstract structures are the items that get sampled
(define (generate-tree types object)
  
  )
(define (find-common-sub-trees trees)
  '())
;;;add operators by selecting the "best" subtrees found by find-subtrees
(define (create-types subtrees)
  '())

