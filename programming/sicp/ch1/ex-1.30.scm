;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-z-h-12.html#%_thm_1.30

;; Iterative sum solution
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
