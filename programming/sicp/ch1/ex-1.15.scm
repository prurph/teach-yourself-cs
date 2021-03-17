;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.15

;; 1.15(a)
(define (cube x) (* x x x))
(define (p x)
  (display "p")
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; Evaluation of (sine 12.15)
;;   (p (sine (/ 12.15 3.0)))
;;   (p (p (sine (/ 4.05 3.0))))
;;   (p (p (p (sine (/ 1.35 3.0)))))
;;   (p (p (p (p (sine (/ 0.45 3.0))))))
;;   (p (p (p (p (p (sine (/ 0.15 3.0)))))))
;;   (p (p (p (p (p 0.05)))))
;; Thus p is applied 5 times, from 12.15 / (3 * 5) < 0.1, plus 

;; 1.15(b)
;; The growth in space and steps increases by one every time a increases by a
;; factor of 3 (because we iteratively divide by 3). In other words:
;;   a / 3^n < 0.1              for angle a and iteration count n
;;   a / 0.1 < 3^n              rearrange
;;   log a - log 0.1 < n log 3  take log and expand
;;   (log a + 1) / log 3 < n    rearrange
;; Thus logarithmic space and time complexity.
