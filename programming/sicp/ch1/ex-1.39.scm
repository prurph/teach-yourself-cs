;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.39

;; cont-frac from ex-1.37
(define (cont-frac n d k)
  (define (rec i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; J.H. Lambert's continued fraction representation of the tangent function:
;;
;;   tan x = x / ( 1 - x^2 / (3 - x^2 / (5 - x^2 ...
;;
;;   N_i = i == 1 ? x : -x^2 (note negative)
;;   D_i = 2i - 1
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= 1 i) x (* x (- x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))
