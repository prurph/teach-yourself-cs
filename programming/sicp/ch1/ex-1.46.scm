;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.46

(define (iterative-improve improve good-enough?)
  (lambda (guess) 
    (if (good-enough? guess)
        guess
        ((iterative-improve improve good-enough?) (improve guess)))))

;; sqrt in terms of iterative-improve
(define (sqrt x)
  (let* ((tolerance 0.00001)
         (first-guess 1.0)
         (improve
           (lambda (guess)
             (/ (+ guess (/ x guess))
                2)))
         (good-enough?
           (lambda (guess)
             (< (abs (- (* guess guess) x)) tolerance))))
    ((iterative-improve improve good-enough?) first-guess)))

;; fixed-point in terms of iterative-improve
(define (fixed-point f first-guess)
  (let* ((tolerance 0.00001)
         (improve f) ; improve guesses by repeatedly applying the input function
         (good-enough?
           (lambda (guess)
             (< (abs (- guess (f guess))) tolerance))))
    ((iterative-improve improve good-enough?) first-guess)))
