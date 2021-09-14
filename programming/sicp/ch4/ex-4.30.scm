#lang sicp

(#%require "lazy-evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.30

;; 4.30(a)
;; The proc to for-each is:

(lambda (x) (newline) (display x))

;; This contains the primitive procedure `display`, which requires the value of
;; x to be forced, therefore no explicit forcing is required.

;; 4.30(b)
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; With the original eval-sequence:

(p1 1)
;; => (1 2)
(p2 1)
;; => 1

;; In the first case, we're evaluating the primitive procedure set!, which gets applied and sets x to (1 2)
;; In the second case we're applying the compound procedure p, so eval-sequence
;; is invoked. For the original eval-sequence, the arguments are delayed, and
;; the set! (argument e to p) is a thunk taht is not evaluated because it is
;; not needed.

;; With Cy's version, that uses actual-value to force all expressions:
(p1 1)
;; => (1 2)
(p2 1)
;; => (1 2)

;; The first case is identical, but now since all expressions are forced, the
;; thunk e is forced, setting x to (1 2).

;; 4.30(c)
;; actual-value calls force-it, which is smart enough to only force thunks, and
;; to simply return normal values

;; 4.30(d)
;; The point of a lazy evaluator is that it's lazy, so even though it may be
;; counter-intuitive, if a result is not required, it seems it should not be
;; evaluated just for side-effects. This speaks to why pure functions and
;; laziness go well together.
