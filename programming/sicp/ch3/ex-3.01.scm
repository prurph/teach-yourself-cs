#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.1

(define (make-accumulator value)
  (lambda (increase)
    (set! value (+ value increase))
    value))

;; Primitives are pass by value, so passing the same reference to a numeric
;; value results in distinct accumulators.
(define initial 10)
(define A (make-accumulator initial))
(define B (make-accumulator initial))

(A 10)
;; => 20
(A 10)
;; => 30
(B 10)
;; => 20
initial
;; => 10
