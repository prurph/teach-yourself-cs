#lang scheme

;; Accumulate as effectively a right fold
;; Exercise 2.38 mentions this explicitly.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
