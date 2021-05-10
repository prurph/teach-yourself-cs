#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.47

(#%require sicp-pict)

;; Supply appropriate selectors to produce an implementation for frames given
;; the following two possible constructors:

;; 1. list constructor
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

;; 2. cons constructor
(define (make-frame origin edge1 edge2)
  (cons (origin (cons edge1 edge2))))

(define origin-frame car)

(define origin-frame cadr)

;; Different: cddr vs caddr
(define edge2-frame cddr)
