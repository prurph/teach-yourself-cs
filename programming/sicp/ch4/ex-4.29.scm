#lang sicp

(#%require "lazy-evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.29

;; The fibonacci example from earlier is a good case where memoization of
;; thunks is useful so that we aren't constantly re-evaluting thunks on the
;; recursive calls.


(driver-loop)
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

;; If the evaluator memoizes, the result is one because the argument (id 10) is
;; evaluated once (as a thunk, then memoized)

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1

;; If the evaluator does not memoize, the result is two because (id 10) is evaluated twice
;; (still lazily as a thunk, but not memoized so (id x) is called twice).

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2
