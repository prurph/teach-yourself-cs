#lang scheme

;; Accumulate as effectively a right fold
;; Exercise 2.38 mentions this explicitly.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Defined as part of Exercise 2.36.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(provide accumulate)
(provide accumulate-n)
