#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.6

;; Church numerals
;; This shit is mindbending. Here's a good resource for the implementations in
;; Scheme: https://gist.github.com/nicky-zs/8296596

;; They key here is that a church numeral is a function that takes a function
;; and returns a new function. Numeral n just applies f(x) n times on x.
(define zero (lambda (f) (lambda (x) x)))    ; apply f zero times
;; add-1 is called "succ" for successive elsewhere
(define (add-1 n)
  (lambda (f)           ; return a function that takes a function
    (lambda (x)         ; that takes an argument x
      (f ((n f) x)))))  ; applies the church numeral n to it

;; Define one by evaluating (add-1 zero)
(add-1 zero)
;; Expand zero
(add-1 (lambda (f) (lambda (x) x)))
;; Expand add-1, using g and y to disambiguate the scopes
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y) f) x)))))
(lambda (f) (lambda (x) (f (lambda (y) y) x)))
(lambda (f) (lambda (x) (f x)))
;; Thus the definition of one is a function that takes a function f and applies
;; it once to its input x.
(define one (lambda (f) (lambda (x) (f x))))

;; Now define two by evaluating (add-1 one)
(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f ((lambda (g) (lambda (y) (g y)) f) x))))
(lambda (f) (lambda (x) (f (((lambda (y) (f y)) x)))))
(lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; Addition procedure
;; Apply n to f (remember its a function that's applying `add-1` n times
;; Apply m to f
;; Apply the n f's to x then apply the m f's
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;; To test this stuff apply a function to the church numeral (itself a function)
;; to get something that will operate on regular numbers:
((zero inc) 0)    ; Create a function that increments zero times, apply to 0
((one inc) 1)     ; Function that increments one time, apply to 1 to get 2
((add one two) 6) ; Function that adds 3, applied to 6 to get 9
