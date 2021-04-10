;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.31

;; If the tree is a leaf, invoke the function on it, otherwise map over it,
;; invoking tree-map on each sub-tree
(define (tree-map f tree)
  (if (not (pair? tree))
      (f t)
      (map (lambda (t) (tree-map f t)) tree)))

;; Alternative definition with lambda at the "top"
(define (tree-map f tree)
  (lambda (t)
    (if (not (pair? t))
        (f t)
        (tree-map f t))))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

(define tree-a 
  (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))

(square-tree tree-a)
;; => '(1 (4 (9 16) 25) (36 49) 100)
