#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.18

;; Using O(n) space instead of fast/slow pointer method.
(define contains-cycle?
  (let ((visited '()))
    (lambda (x)
      (cond ((memq x visited) #t)
            ((null? (cdr x)) #f)
            ((set! visited (cons x visited))
             (contains-cycle? (cdr x)))))))

(define no-cycle (list 1 2 3))

(define circular
  (let ((l1 (list 1 2 3)))
    (set-cdr! (cddr l1) l1)
    l1))

(define inner-loop
  (let ((l1 (list 1 2 3)))
    (set-cdr! (cddr l1) (cdr l1))
    l1))
