;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.9

(define (dec x)
  (- x 1))

(define (inc x)
  (+ 1 x))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; Evaluation for (+ 4 5)
;;   (+ 4 5)
;;   (inc (+ 3 5))
;;   (inc (inc (+ 2 5)))
;;   (inc (inc (inc (+ 1 5))))
;;   (inc (inc (inc (inc (+ 0 5)))))
;;   (inc (inc (inc (inc 5))))
;;   (inc (inc (inc 6)))
;;   (inc (inc 7))
;;   (inc 8)
;;   9
;; This is a recursive process: in addition to the values of a and b, the stack
;; must be used to know how many inc calls we have remaining. This makes sense
;; because the defined procedure is *not* tail-recursive (last call is to
;; `inc`, not +).

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; Evaluation for (+ 4 5)
;;   (+ (dec 4) (inc 5))
;;   (+ 3 6)
;;   (+ 2 7)
;;   (+ 1 8)
;;   (+ 0 9)
;;   9
;; This is an iterative process: the arguments are recomputed at each step, and
;; the state of the program is contained entirely within a and b. This makes
;; sense because the defined procedure is tail-recursive (last call is to the
;; procedure itself).
