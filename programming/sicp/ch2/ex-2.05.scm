#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.5

;; Represent pairs of non-neg integers a and b as the integer 2^a * 3^b
;; Cool note: this works for any relatively prime numbers.
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; "Pull out" 2's until there is a remainder; `car` is the number of 2's pulled
;; out.
(define (car c)
  (define (iter c n)
    (if (zero? (remainder c 2))
        (iter (/ c 2) (inc n))
        n))
  (iter c 0))

;; `cdr` is identical but pull out 3's
(define (cdr c)
  (define (iter c n)
    (if (zero? (remainder c 3))
        (iter (/ c 3) (inc n))
        n))
  (iter c 0))
