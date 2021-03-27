;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.7

(load "interval.scm")

(define (upper-bound i)
  (cdr i))
(define (lower-bound i)
  (car i))
