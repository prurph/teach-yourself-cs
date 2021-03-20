;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.37

;; Return the "k-term finite continued fraction", meaning:
;;   N1 / (D1 + (N2 / (D2 + N3 / (D3 + ...))))
;; where n and d are single-arg procedures that return N_i and D_i of the
;; continued fraction for index i

;; 1.37(a)
;; Recursive solution: the i-th term is
;;   (n i)/(d i)         for i = k
;;   (n i) + (d i + ...) for i < k
;; Start with i = 1 and work "down"
(define (cont-frac n d k)
  (define (rec i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; 1.37(b)
;; Iterative solution
;; Start with (n k)/(d k) term and work "back up"
(define (cont-frac n d k)
  (define (dec x) (- x 1))
  (define (iter result i)
    (if (= 0 i)
        result
        (iter ( / (n i) (+ (d i) result)) (dec i))))
  (iter (/ (n k) (d k)) (dec k)))
