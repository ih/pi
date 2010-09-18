(library (generate-tree)
         (export generate-tree)
         (import (rnrs))

         (define (initialize ?)
           (
         (define (generate-tree types object)
           (if (primitive? object)
               object
               (let* ((syntactic-structure (choose-structure type))
                      (new-node (node syntactic-structure))
                      (children-info (children-of syntactic-structure)))
                 (cons new-node (map-list-args generate-tree children-info)
         
         
         )