;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.16

;; Use an accumulator a, begin with a = 1, therefore ab^n = b^n. Manipulate
;; a with each iteration such that the ab^n remains constant, when b^n = 1,
;; return a as the final answer.
(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ;; If n is even:
          ;;   ab^n = a(b^(n/2))^2 = a(b^2)^(n/2)
          ;;   a -> a  // b -> b^2 // n -> n/2
          ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
          ;; If n is odd:
          ;;   ab^n = abb^(n-1) = (ab)b^(n-1)
          ;;   a -> ab // b -> b // n -> n - 1
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter b n 1))
