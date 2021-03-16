;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.11

;; Recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; Iterative process
(define (f n)
  ;; Current iteration will calculate the nth term, so when nth exceeds n,
  ;; return f(n - 1)
  (define (f-iter nth fn-1 fn-2 fn-3)
    (if (> nth n)
        fn-1
        (f-iter (inc nth)
                (+ fn-1
                   (* 2 fn-2)
                   (* 3 fn-3))
                fn-1
                fn-2)))
  (if (< n 3)
      n
      (f-iter 3 2 1 0)))
