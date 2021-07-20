#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.76

(define (sign-change-detector v1 v2)
  (cond ((and (> v1 0) (< v2 0)) -1)
        ((and (< v1 0) (> v2 0))  1)
        (else 0)))

;; I originally had (add-streams s (stream-cdr s)) but I think using 0 is better
;; in this case in the sense that each "position" in the input stream should
;; correspond to one in the smoothed output.
(define (smooth s)
  (scale-stream (add-streams s (cons-stream 0 s)) (/ 1 2)))

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector
              input-stream
              (cons-stream 0 input-stream)))

(define sense-data (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings
  (make-zero-crossings (smooth sense-data)))

(display-stream zero-crossings)
