;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.20

;; Simply scheme filter operates on simply-scheme lists made with `se`
(define (filter-cons p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter-cons p (cdr l)))))

(define (same-parity a . rest)
  (filter-cons (lambda (x) (equal? (even? x) (even? a))) (cons a rest)))


