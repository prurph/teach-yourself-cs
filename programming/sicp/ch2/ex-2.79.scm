#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.79

(define (install-generic-equality)
  (define (equ-rat? x y) 
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (equ-complex? x y)
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) equ-rat?)
  (put 'equ? '(complex complex) equ-complex?)
  'done)

(define (equ? x y) (apply-generic 'equ? x y))
