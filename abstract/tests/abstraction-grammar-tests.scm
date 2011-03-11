(import (abstract)
        (srfi :78))

(let* ([start-rule (make-start-rule '(F1))]
       [rule-F1 (make-function-rule 'F1 '(a (a (V25) (V25))))]
       [rule-V25 (make-variable-rule 'V25 '(b c d))]
       [grammar (make-grammar start-rule (list rule-F1 rule-V25))])
 (check ((eval grammar)) => '(a (a (b) (b)))))

