#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.23

;; One option: use begin to run multiple expressions in else block
(define (for-each f l)
  (if (null? l)
      (void)
      (begin (f (car l))
             (for-each f (cdr l)))))

;; Another option: use `cond`, which can contain more than two expressions
(define (for-each f l)
  (cond ((null? l) (void))
        (else (f (car l))
              (for-each f (cdr l)))))
