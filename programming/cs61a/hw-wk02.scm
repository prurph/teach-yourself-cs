#lang scheme
;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week2)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46 (see SICP solutions)

;; 2.
;; Unconventional naming: they suggest you write map and call it every
(define (every f xs)
  (if (empty? xs)
      '()
      (se (f (first xs)) (every f (bf xs)))))

;; 3.
;; simply-scheme in fact defines map (as every) and filter (as keep)
(every (lambda (number)
         (if (even? number)
             (word number number)
             number))
       '(781 5 76 909 24))
;; '(781 5 7676 909 2424)

;; Extra
;; TIL: the "Y Combinator" is a computer science thing!
;; It has the form:
;;   (lambda (f) (lambda (n) (f f n))))
;; This is super mindblowing! The Y Combinator is a function that, when
;; invoked with a function, returns a function that takes arguments and
;; applies them to that function, while also providing a reference to the
;; function itself, making recursion possible.
;;
;; So to make a function into a "Y Cominatorable" one, you just add an
;; additional argument to it, eg (lambda (fn x ...)) and then call the YC with
;; it. The `(f f n)` part of the YC, then, is this function being called with
;; its argument (`n` in this case) and that self reference, `f`.

;; Factorial using the Y Combinator
((lambda (f) (lambda (n) (f f n)))
 (lambda (fn x)
   (if (equal? x 0)
       1
       (* x (fn fn (- x 1))))))
