#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.19

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; The footnote essentially gives the "answer". In a perfect world (inner) a and b would be defined simultaneously, giving (+ 5 x) for b, and returning (+ 5 (+ 5 x)) = 10 + x, or 20 for (f 10), but this is difficult to implement and therefore the above will error; in Scheme you'll see "a: undefined; cannot use before initialization".
