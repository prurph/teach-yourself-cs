#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.45

(#%require sicp-pict)

;; Implement `split` such that the definitions for `right-split` and `up-split`
;; are valid. At first I just did:
;;   (lambda (painter) (major painter (minor painter painter)))
;; but that doesn't handle the count argument.
(define (split major minor)
  (define (split-rec painter n)
    (if (= 0 n)
        painter
        (let ((smaller (split-rec painter (- n 1))))
          (major painter (minor smaller smaller)))))
  split-rec)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 2))
(paint (up-split einstein 2))
