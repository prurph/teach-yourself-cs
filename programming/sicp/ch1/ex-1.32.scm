;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.32

;; 1.32(a)
;; Provide a monoid with:
;;   - combine -> combiner
;;   - empty -> null-value
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; 1.32(b)
;; accumulate above is recursive, iterative solution:
(define (acccumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        null-value
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
