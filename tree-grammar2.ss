(generate-syntax-tree 3 trees 'T)

(define trees '((T (create-node C L))
                (L (list T) (append L L) null)
                (C a b c d e f)
;;                (TIF (make-if T T))



                
(define (generate-syntax-tree time grammar rule-name)
  (make-evalable grammar (generate-partial-syntax-tree time grammar rule-name)))


(define (generate-partial-syntax-tree time grammar rule-name)
  (let* ((current-rule (select-rule grammar rule-name))
                                        ;(db (pretty-print (list "db1" time rule-name current-rule)))
         (operation (choose-operation current-rule))
                                        ;(db (pretty-print (list "db2" time rule-name operation)))
         )
    (if (and (has-operands? operation) (not (times-up? time)))
        (construct-operation (get-operator operation) (map (curry generate-partial-syntax-tree (adjust-time time) grammar) (get-operands operation)))
        operation)))

(construct-operation (get-operator operation) (map (curry generate-partial-syntax-tree (adjust-time time) grammar) (get-operands operation)))
            operation)))

;;;operation implementation functions; operations are implemented as a list with the first element being an operator (lambda syntax-tree) and the rest of the elements are the operands, within a rule the operands are represented by rule names 

(define (has-operands? op)
  (and (list? op) (not (null? op))))

                                        ;construct-operation takes two arguments, the operator and a list of operands
(define construct-operation pair)

(define get-operator first)

(define get-operands rest)

(define (operator? op)
  (and (list? op) (not (null? op))))


;;for now make-evalable turns rule names into 1s, this can be generalized to replacing the rule names with primitives from the type represented by the rule name

(define (make-evalable grammar partial-syntax-tree)
      (let ((rule-names (get-rule-names grammar)))
        (make-evalable-recursion rule-names partial-syntax-tree)))

(define (make-evalable-recursion rule-names partial-syntax-tree)
  (if (operator? partial-syntax-tree)
      (let* ((operands (get-operands partial-syntax-tree)))
        (construct-operation (get-operator partial-syntax-tree) (map (curry make-evalable-recursion rule-names) operands)))
      (if (member? partial-syntax-tree rule-names)
                                        ;domain specific code; pick a primitive from the type
          (cond ((equal? partial-syntax-tree 'C) (uniform-draw (list 'a 'b 'c 'd 'e 'f)))
                ((equal? partial-syntax-tree 'L) '())
                ((equal? partial-syntax-tree 'T) '(create-node 'a '())))
          partial-syntax-tree)))


(define (create-node color tail)
  (pair color tail))
(define a 'a)
(define b 'b)
(define c 'c)
(define d 'd)
(define e 'e)
(define f 'f)
(define null '())
