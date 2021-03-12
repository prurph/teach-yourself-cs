;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-z-h-12.html#%_thm_1.31

;; 1.31(a)
;; Identical to sum from 1.29 except using the multiplication monoid instead of
;; addition.
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product * 1 (lambda (x) (+ 1 x)) n))

(define (approx-pi k)
  ;; numerators go   2, 4, 4, 6, 6, 8, 8 -> (if (odd? a)  (+ a 1) (+ a 2))
  ;; denominators go 3, 3, 5, 5, 7, 7    -> (if (even? a) (+ a 1) (+ a 2)))
  (define (term a)
    (if (odd? a)
        (/ (+ a 1) (+ a 2))
        (/ (+ a 2) (+ a 1))))
  (* 4.0 (product term 1 (lambda (x) (+ 1 x)) k)))

;; 1.31(b)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
