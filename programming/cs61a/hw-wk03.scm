;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week3)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP 1.16, 1.35, 1.37, 1.38

;; 2.
;; Find the next perfect number after n, where a perfect number is a number
;; equal to the sum of all its factors less than itself (eg 6 = 1 + 2 + 3)

;; Find the factors less than n
(define (factors n)
  (define (iter i acc)
    ;; If i is a factor, add both i and n/i. This implies we're done when i
    ;; exceeds sqrt n.
    (cond ((> i (sqrt n)) acc)
          ((= i (sqrt n)) (se acc i))
          ((zero? (modulo n i)) (iter (+ i 1) (se acc i (/ n i))))
          (else (iter (+ i 1) acc))))
  (iter 2 '(1)))

(define (is-perfect? n)
  (= n (reduce + (factors n))))

(define (next-perf n)
  (if (is-perfect? (+ n 1))
      (+ n 1)
      (next-perf (+ n 1))))

(next-perf 29)
;; => 496
