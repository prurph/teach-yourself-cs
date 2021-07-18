#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.68

(define (pairs-louis-reasoner s t)
  (interleave (stream-map (lambda (x) (list (stream-car s) x))
                          t)
              (pairs-louis-reasoner (stream-cdr s) (stream-cdr t))))

;; This recurses infinitely because there is no base case. When pairs is called
;; the arguments to interleave must be evaluated, which calls pairs, which
;; evaluates the arguments to interleave, which calls pairs, which... The key is
;; that this doesn't use cons-stream, which is a special form that allows
;; delayed execution.
