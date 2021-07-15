#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.51

(define (show x)
  (newline)
  (display x)
  x)

;; Console output from the following commands:
(define x (stream-map show (stream-enumerate-interval 0 10)))
;; => 0
(stream-ref x 5)
;; => 1
;; => 2
;; => 3
;; => 4
;; => 5
(stream-ref x 7)
;; => 6
;; => 7

;; Since cons-stream is (cons a (delay b)), the show procedure is only invoked
;; on each successive car as it is required, and further delay memoizes the
;; result, so previously computed elements need not be recomputed.
