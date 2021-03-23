;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.38

;; cont-frac from ex-1.37
(define (cont-frac n d k)
  (define (rec i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; Euler's continued fraction for e-2:
;;   N_i is always 1
;;   D_i is 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8
(define (approx-e k)
  (define (d i)
    (cond ((= (modulo i 3) 2) (* 2 (/ (+ i 1) 3)))
          (else 1)))
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))

(approx-e 100)
