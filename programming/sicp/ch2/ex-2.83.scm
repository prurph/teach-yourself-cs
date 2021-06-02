#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.83

(require "generic-arithmetic.scm")

;; integer -> rational -> real
;; Assuming mae-rational, make-real, numer, denom, etc. all exist already

(define (integer->rational i)
  (make-rational i 1))
(put 'raise '(integer) integer->rational)

(define (rational->real r)
  (make-real (/ (numer r) (denom r))))
(put 'raise '(rational) rational->real)

(define (raise x)
  (apply-generic 'raise x))
