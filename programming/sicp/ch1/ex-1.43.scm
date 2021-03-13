;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.43

(define (repeated f n)
  (if (equal? 1 n)
      f
      (repeated (lambda (x) (f (f x))) (- n 1))))

;; or using compose
(define (repeated f n)
  (if (equal? 1 n)
      f
      (repeated (compose f f) (- n 1))))

(define (square x) (* x x))

((repeated square 2) 5) ; 625
