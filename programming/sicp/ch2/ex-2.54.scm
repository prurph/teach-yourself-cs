#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.54

;; Cheap xor that only accepts two arguments
(define (xor a b)
  (if (not (and (boolean? a) (boolean? b)))
      (error "xor called with non-boolean argument")
      (not (eq? a b))))

;; Could also implement with pair? and null?
(define (my-equal? l1 l2)
  (cond ((xor (list? l1) (list? l2)) #f)
        ((and (list? l1) (empty? l1)) (empty? l2))
        ((not (list? l1)) (eq? l1 l2))
        (else (and (my-equal? (car l1) (car l2))
                   (my-equal? (cdr l1) (cdr l2))))))
