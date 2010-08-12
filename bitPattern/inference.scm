;data is a file with a list of bit strings, but start by passing a list directly
(define (inferModel language data)
  (let ((bitStrings (parseData data)))
    (performAbstraction language (createInitialModel language bitStrings))))



(define (parseData data)
  data)

(parseData '(1 2 3))


;a language is defined by its syntax and semantics
(define-record-type language
  (fields syntax semantics))

;createInitialModel builds abstract syntax trees that are literal translations of the data
(define (createInitialModel language data)
  (if (null? data)
      '()
      (cons (deinterpret (first data) (semantics language)) (createInitialModel language (rest data)))))

;performAbstraction attempts to match syntax trees and replace parts that vary a lot with variables
(define (performAbstraction language initialModels)
  (let ((syntaxTrees (parse initialModels))