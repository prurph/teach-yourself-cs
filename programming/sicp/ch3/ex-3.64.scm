#lang sicp

(#%require "stream.scm")
(#%require "ex-3.63.scm") ; sqrt-stream


;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.64

;; My first inclination was to use stream filter but the lambda therein is
;; provided with the each element of the stream, not the stream itself.
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.01)
;; => 1.4142156862745097
