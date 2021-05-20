#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.62

;; Operations on sets represented as lists of unique ascending numbers

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (cons (car set2)
                                         (union-set set1 (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) set2)))))
