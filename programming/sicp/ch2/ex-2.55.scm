#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.55

;; Explain the following:
> (car ''abc)
'quote

;; ' is the special form of a procedure `quote`
(car ''abc)               ;; interpreter reads as:
(car (quote (quote abc))) ;; rewriting the first quote:
(car '(quote abc))        ;; this is the list literal (list quote abc)
;; The car of the list (list quote abc) is just quote
