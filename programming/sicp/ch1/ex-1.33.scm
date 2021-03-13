;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-z-h-12.html#%_thm_1.33

;; Recursive version of filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

;; Iterative version of filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

;; 1.33(a)
;; Janky prime? function
(define (prime? n)
  (define (prime-iter i)
    (cond ((>= i n) #t)
          ((zero? (modulo n i)) #f)
          (else (prime-iter (+ 1 i)))))
  (and (> n 1) (prime-iter 2)))

(define (sum-square-primes a b)
  (define (square x) (* x x))
  (define (inc x) (+ 1 x))
  (filtered-accumulate + 0 square a inc b prime?))

;; 1.33(b)
(define (gcd x y)
  ;; Note that if (> y x), the first iteration evaluates as:
  ;;  (gcd x (modulo y x)) = (gcd x y)
  ;; thus the order of the arguments does not matter.
  (if (zero? (modulo x y))
      y
      (gcd y (modulo x y))))

(define (relatively-prime? x y)
  (equal? 1 (gcd x y)))

(define (product-relative-primes n)
  (define (filter x) (relatively-prime? n x))
  (define (inc x) (+ 1 x))
  (filtered-accumulate * 1 identity 1 inc n filter))
