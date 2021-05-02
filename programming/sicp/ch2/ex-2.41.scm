#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.41

(require "accumulate.scm")
;; flatmap and enumerate-interval
(require "ex-2.40.scm")

;; Return all ordered triples of distinct positive integers i, j, and k less
;; than or equal to n.
(define (ordered-distinct-triples n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                                 (enumerate-interval (+ 1 j) n)))
                                (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n)))

(define (list-sum l)
  (accumulate + 0 l))

;; Return all ordered triples of distinct positive integers i, j, and k less
;; than or equal to n that sum to s
(define (distinct-triples-lt-sum-to n s)
  (filter (lambda (triple) (equal? (list-sum triple) s))
          (ordered-distinct-triples n)))
