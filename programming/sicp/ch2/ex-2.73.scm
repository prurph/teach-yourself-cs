#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-17.html#%_thm_2.73

(require "deriv.scm")
(require "ex-2.56.scm") ; exponentiation derivatives

;; 2.73(a)
;; Each expression has a separate `deriv` procedure, tagged by its operator. We
;; look that up from the dispatch table and then apply it. Since `number?` and
;; `variable?` do not use operators or apply to operands, they cannot be
;; assimilated into the generic dispatch mechanism. Moreover, they are
;; predicates that apply to arguments of any type, therefore this wouldn't even
;; necessarily be advantageous.

;; 2.73(b)
(define (put) '())
(define (get) '())

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))

(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)

;; 2.73(c)
(define (deriv-exponentiation exp var)
  (let ((b (base exp))
        (e (exponent exp)))
    (make-product (make-product e
                                (make-exponentiation (base exp) (make-sum e -1)))
                  (deriv b var))))

(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

;; 2.73(d)
;; This is tantamount to flipping the rows/columns of the dispatch table, such that now the operator is provided first, then the operation ('deriv). This only requires a small change to the put procedures in the installation:
(define (install-deriv)
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '** 'deriv deriv-exponentiation)
  'done)
