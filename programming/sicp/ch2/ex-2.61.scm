#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.61

;; Operations on sets represented as lists of unique ascending numbers

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              (else (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((eq? x (car set)) set)
        ;; If x is less than the next, it must go here since set is ordered
        ((< x (car set)) (cons x set))
        ;; If x is greater than the next, we haven't reached its position yet
        (else (cons (car set) (adjoin-set x (cdr set))))))
