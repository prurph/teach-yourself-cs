#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.74

(define (sign-change-detector v1 v2)
  (cond ((and (> v1 0) (< v2 0)) -1)
        ((and (< v1 0) (> v2 0))  1)
        (else 0)))

(define sense-data (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

;; The original version is incorrect because it detects changes between the
;; average and the prior value, not the last two averages.
(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream)
                           last-value)
                        2)))
        (cons-stream (sign-change-detector avpt last-avg)
                     (make-zero-crossings (stream-cdr input-stream)
                                          (stream-car input-stream)
                                          avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))
(display-stream zero-crossings)
