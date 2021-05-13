#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.48

(#%require sicp-pict)


;; Direccted line segment as a pair of vectors from origin to start and origin
;; to end
(define (make-segment) cons)

(define (start-segment) car)

(define (end-segment) cdr)
