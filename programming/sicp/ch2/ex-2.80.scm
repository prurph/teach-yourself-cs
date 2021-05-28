#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.80

(define (install-generic-zero?)
  (define (=zero-rat? x) (zero? (numer x)))
  (define (=zero-complex? x) (zero? (magnitude x)))
  (put '=zero? '(scheme-number) zero?)
  (put '=zero? '(rational) =zero-rat?)
  (put '=zero? '(complex) =zero-complex?)
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))
