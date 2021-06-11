#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.19

;; Constant space. The classic fast/slow pointer.
(define (contains-cycle? l)
  ;; p2 is the fast pointer and thus will always hit any end of a list first,
  ;; implying we needn't perform any null checks on p1.
  (define (go p1 p2)
    (cond ((null? p2) #f)
          ((null? (cdr p2)) #f)
          ((eq? p1 p2) #t)
          (else (go (cdr p1) (cddr p2)))))
  (if (or (null? l)
          (null? (cdr l))
          (null? (cddr l)))
      #f
      (go (cdr l) (cddr l))))

(define no-cycle (list 1 2 3))

(define circular
  (let ((l1 (list 1 2 3)))
    (set-cdr! (cddr l1) l1)
    l1))

(define inner-loop
  (let ((l1 (list 1 2 3)))
    (set-cdr! (cddr l1) (cdr l1))
    l1))

(define unicycle
  (let ((l1 (list 1)))
    (set-cdr! l1 l1)
    l1))


(contains-cycle? no-cycle)
(contains-cycle? circular)
(contains-cycle? inner-loop)
(contains-cycle? unicycle)
