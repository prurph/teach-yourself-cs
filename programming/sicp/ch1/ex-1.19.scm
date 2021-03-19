;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.19

;; Generate Fibonacci numbers by the transform T: a <- a + b and b <- a
;; b = fib(n) and a = fib(n + 1) are generated from applying T^n (n times) to the
;; starting pair (1, 0)
;; Consider this T as a special case of Tpq, for p = 0 and q = 1
;;   a <- aq + bq + ap
;;   b <- aq + bp
;; Applying Tpq twice is:
;;   1) a' <- aq + bq + ap
;;      b' <- aq + bp
;;   2) a" <- a'q + b'q + a'p = (aq + bq + ap)q + (aq + bp)q + (aq + bq + ap)p
;;                            = a(q^2 + 2pq) + b(q^2 + 2pq) + a(q^2 + p^2)
;;      b" <- a'q + b'p = (aq + bq + ap)q + (aq + bp)p
;;                      = a(q^2 + 2pq) + b(q^2 + p^2)
;; From the above deconstruction, Tpq = Tp'q`, where:
;;   p' = (q^2 + p^2)
;;   q' = (q^2 + 2pq)

(define (fast-fib n)
  (define (square x) (* x x))
  (define (fast-fib-iter a b p q n)
    (cond ((= n 0) b)
          ;; If n is even, we can apply T twice in one procedure using the
          ;; above derived p' and q'
          ((even? n) (fast-fib-iter a
                                    b
                                    (+ (square q) (square p))
                                    (+ (square q) (* 2 p q))
                                    (/ n 2)))
          ;; Otherwise, apply T once to get new a and b (p and q don't change)
          (else (fast-fib-iter (+ (* b p) (* a q) (* a p))
                               (+ (* b p) (* a q))
                               p
                               q
                               (- n 1))))) ; b is fib(n), a is fib(n + 1)
  (fast-fib-iter 1 0 0 1 n))
