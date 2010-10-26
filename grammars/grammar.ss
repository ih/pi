(import (_srfi :1)
        (church readable-scheme))


;;                (TIF (make-if T T))


;;;general functions
(define (all lst)
  (if (null? lst)
      #t
      (and (first lst)
           (all (rest lst)))))
(define (curry fun . args)
  (lambda x
    (apply fun (append args x))))

;;;generate-expressions from a grammar
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
      (if (member partial-syntax-tree rule-names)
                                        ;domain specific code; pick a primitive from the type
          (cond ((equal? partial-syntax-tree 'C) (uniform-draw (list 'a 'b 'c 'd 'e 'f)))
                ((equal? partial-syntax-tree 'L) ''())
                ((equal? partial-syntax-tree 'T) '(create-node 'a '())))
          partial-syntax-tree)))

;;;grammar implementation functions; a grammar is implemented as a list of rules
(define (select-rule grammar rule-name)
  (find (lambda (rule) (equal? (get-rule-name rule) rule-name)) grammar))

(define (get-rule-names grammar)
  (map first grammar))

(define add-rule pair)

(define (get-first-rule-name grammar)
  (first (get-rule-names grammar)))

;;;rule implementation functions; rules are implemented as a list with the first element being the name of the rule and the rest of the elements are operations

(define construct-rule pair)

(define get-rule-name first)

;;rules should be modified to have a parameterized distribution over operations
(define (choose-operation rule)
  (uniform-draw (get-operations rule)))

(define get-operations rest)

(define (circular? rule)
  (let* ((rule-name (get-rule-name rule))
         (rhs (get-rule-rhs rule)))
    (all (map (curry self-reference? rule-name) rhs))))

(define (self-reference? rule-name node)
  (if (operator? node)
      (let ((operands (get-operands node)))
        (member? rule-name operands))
      (equal? node rule-name)))


(define (make-non-cicular rule old-names)
  (construct-rule (get-rule-name rule) (pair (uniform-draw old-names) (get-rule-rhs rule))))

(define get-rule-rhs rest)

;;;bounded generation related functions
(define (times-up? time) (= 0 time))
(define (adjust-time time) (- time 1))


;;tree grammar specifics
(define (create-node color tail)
  (append `(node ,color) tail))

(define (function-call function-name)
  (append function-name args)

(define (make-if tree1 tree2)
  `(if (flip)
      ,tree1
      ,tree2))
(define a 'a)
(define b 'b)
(define c 'c)
(define d 'd)
(define e 'e)
(define f 'f)
(define null '())

(define program '((P (create-program DL B))
                  (DL (list D) (append D D) null)
                  (D (create-definition))
                  (B (create-body))))

(define trees '((T (create-node C L) (make-if T T) (function-call FN))
                (L (list T) (append L L) null)
                (C a b c d e f)))

(define node list)


(eval (generate-syntax-tree 3 trees 'T) (interaction-environment))