#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.30

;; Defining square-tree for trees represented by nested lists
(define tree-a 
  (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))

;; Directly without higher-order procedures
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree tree-a)
;; => '(1 (4 (9 16) 25) (36 49) 100)

;; Recursively with map, treating each element as a sub-tree
(define (square-tree tree)
  (map (lambda (t)
         (if (pair? t)
             (square-tree t)
             (* t t)))
       tree))
