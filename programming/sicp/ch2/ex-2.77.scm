#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.77

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; The above work because complex numbers are now represented like:
;;   ('complex ('rectangular (3 . 4))
;;   ('complex ('polar (1 0.5)))
;; By adding entries in the table under the "parent" type '(complex) when
;; apply-generic is called on a complex number it will first strip off the
;; '(complex) from mapping its single argument, then find `real-part` for
;; complex numbers per the above definition. The definition of `real-part` is:
;;   (define (real-part z) (apply-generic 'real-part z))
;; Thus there is another apply-generic that ultimately finds the relevant
;; function in the polar or rectangular package.
