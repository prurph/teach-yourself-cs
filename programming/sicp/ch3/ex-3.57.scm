#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.57

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs) fibs))))

;; When computing the nth Fib using the above definition, there are n-1
;; additions performed. If delay did not memoize, there would be O(2^n) since
;; to calculate fibs(n) we'd calculate fibs(n-1) + fibs(n-2), and each of those
;; would in turn duplicate calcuations for n-3, n-4, etc.
