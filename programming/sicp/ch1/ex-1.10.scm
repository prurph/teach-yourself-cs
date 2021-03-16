;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.10

;; Ackermann's function (seems to be a variant?)
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
;; (A 0 (A 1 9))
;; after much expansion
;; (A 0 (expt 2 9)) = 1024
(A 2 4)
;; (A 1 (A 1 4))
;; (A 1 (expt 2 4))
;; (A 1 16)
;; (A 0 (expt 2 15)) = 65536
(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 4) = 65536

(define (f n) (A 0 n))
;; 2n
(define (g n) (A 1 n))
;; 0    n = 0
;; 2^n  n > 0 
(define (h n) (A 2 n)) ;
;; 0                    n = 0
;; 2                    n = 1
;; 2^2^2^... n-1 times  n > 1
