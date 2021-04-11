#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.17

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;; Using existing procedures
(define (last-pair l)
  (list-ref l (- (length l) 1)))
