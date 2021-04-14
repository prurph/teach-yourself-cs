#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.34

(load "accumulate.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;; Sample: 1 + 3x + 5x^3 + x^5 at x = 2
(trace accumulate)
(horner-eval 2 (list 1 3 0 5 0 1))

>(accumulate #<procedure> 0 '(1 3 0 5 0 1))
> (accumulate #<procedure> 0 '(3 0 5 0 1))
> >(accumulate #<procedure> 0 '(0 5 0 1))
> > (accumulate #<procedure> 0 '(5 0 1))
> > >(accumulate #<procedure> 0 '(0 1))
> > > (accumulate #<procedure> 0 '(1))       ; Each step "up" gets mulitplied by x, so this is 1*x^5 in the end
> > > >(accumulate #<procedure> 0 '())
< < < <0
< < < 1
< < <2
< < 9
< <18
< 39
<79

79
