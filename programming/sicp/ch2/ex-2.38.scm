#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.38

;; fold-right and fold-left reproduced for reference
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

;; What are the values of:
(fold-right / 1 (list 1 2 3))

;; Start with 3, it becomes divisor for thing to the right (move from right to left)
>(fold-right / 1 '(1 2 3))
> (fold-right / 1 '(2 3))
> >(fold-right / 1 '(3))
> > (fold-right / 1 '())
< < 1
< <3
< 2/3
<3/2
3/2

;; Start with 1, divide by thing to the right (move from left to right)
(fold-left / 1 (list 1 2 3))
1/6

(fold-right list '() (list 1 2 3))
'(1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
(((() 1) 2) 3)

;; Give a property that `op` should satisfy to guarantee that fold-right and
;; fold-left will produce the same values for any sequence.

;; To produce the same values `op` should be commutative:
(equal? (op a b) (op b a))
