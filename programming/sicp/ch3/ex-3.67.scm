#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.67

;; Modified pairs that produces all pairs of elements from two streams. Now for
;; stream s we also take each element of t in turn and pair it with the
;; remaining s elements.
(define (pairs-cartesian s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (interleave
                             (stream-map (lambda (x) (list x (stream-car t)))
                                         (stream-cdr s))
                             (pairs (stream-cdr s) (stream-cdr t))))))

;; An alternative solution that recursively calls pairs with the full stream t.
;; This is easier to understand because again we form the pairs in three parts:
;;
;; 1. The first column of the first row: (stream-car s) (stream-car t)
;; 2. The rest of the first row: (stream-map ...) on stream-cdr t
;; 3. All remaining rows, now which are formed by combining the remaining s
;;    elements with every t, not just (stream-cdr t).
(define (pairs-cartesian-alt s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s) t))))
