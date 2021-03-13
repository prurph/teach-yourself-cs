;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ 1 x))

(((double (double double)) inc) 5)
;; Creates a function that applies inc 16 times to its argument, so returns 21

;; Explanation:
;;   (((double (double double)) f) x)
;;     -> (((double double) ((double double) f)) x)
;;     -> (((double double) (double (double f))) x)
;;     -> (((double (double (double (double f))))) x)
;;   Applies f 2 * 2 * 2 * 2 = 16 times
;;
;; Alternately, think of doubling (double double), so there are 4 total doubles 
;; so 2 * 2 * 2 * 2
