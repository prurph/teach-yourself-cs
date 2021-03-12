;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.29

;; Sum of (f a) + (f (next a)) + ... + (f b)
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Simpson's Rule: the integral of a function f between a and b is approximated
;; by:
;;   (h/3(y_0 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + y_n)), where
;;     h   = (b-a)/n 
;;     y_k = f(a + kh)
;;     n is an even integer
(define (simpsons-rule f a b n)
  ;; must use let* because y depends on the h defined in this let block
  (let* ((h (/ (- b a) n))
         (y (lambda (k) (f (+ a (* k h))))))
    (define (term k)
      (* (y k)
         ;; coefficient for y_k
         (cond ((or (equal? k 0) (equal? k n)) 1)
               ((even? k) 2)
               (else 4))))
    (* h 1/3 (sum term 0 (lambda (x) (+ 1 x)) n))))
