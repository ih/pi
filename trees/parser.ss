;;;return a program from grammar that produces the data

;;;the memory stores future expansions in the parse tree and are adjusted as different parts of the data get parsed since this influences future parts
(define memory '())
(define (parse grammar data)
  (let ((current-rule (select-rule grammar))