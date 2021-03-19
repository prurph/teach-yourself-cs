;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.18


(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-* a b)
  (define (fast-*-iter a b c)
    (cond ((= b 0) c)
          ;; If b is even:
          ;;   abc = 2a(b/2)c
          ;;   a -> 2a  //  b -> b/2  //  c -> c
          ;; Double a, halve b, accumulator stays the same
          ((even? b) (fast-*-iter (double a) (halve b) c))
          ;; If b is odd:
          ;;   ab + c = a(b - 1) + a + c
          ;;   a -> a  //  b -> b - 1  //  c -> a + c
          ;; Pull out one a (so instead of b we have b -1 left) and add it to
          ;; the accumulator
          (else (fast-*-iter a (- b 1) (+ a c)))))
  (fast-*-iter a b 0))
