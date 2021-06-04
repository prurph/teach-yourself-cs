#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.08

(define return-x-once-then-zero
  (let ((called? #f))
    (lambda (x)
      (if called?
          0
          (begin (set! called? #t)
                 x)))))

(define f return-x-once-then-zero)

;; In my scheme interpreter (racket repl) this returns 0, implying it is
;; evaluated left to right.
(+ (f 0) (f 1))
