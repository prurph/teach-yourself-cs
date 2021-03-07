; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.3
; Define a procedure that takes three numbers as arguments and returns the sum
; of the squares of the two larger numbers.
(define (sum-square-largest-two x y z)
  (cond ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
        ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
        (else (+ (* x x) (* y y)))))

(= 13 (sum-square-largest-two 1 2 3))
(= 2  (sum-square-largest-two 1 1 1))
(= 8  (sum-square-largest-two 1 2 2))
(= 5  (sum-square-largest-two 1 1 2))
(= 25 (sum-square-largest-two 1 4 3))

