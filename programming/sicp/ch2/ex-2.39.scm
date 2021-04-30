#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.39

;; fold-right and fold-left from Exercise 2.38.
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; Since we're moving to the left, we see the list in reverse order.
;; `x` is each successive term, `y` is the accumulator.
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;; Since we're moving to the right, we see the list in order.
;; `x` is now the accumulator, and `y` is each successive term, and must go
;; in front to reverse the order.
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
