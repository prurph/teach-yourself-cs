#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.11

;; The cases are the permutations of each interval being entirely below zero,
;; entirely above zero, or spanning zero.
;; When 
;; The case that requires multiple multiplications is the case where both span
;; zero.
(define ))
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y))
        (xpos? (> 0 xl))
        (xneg? (< 0 xu))
        (ypos? (> 0 yl))
        (yneg? (< 0 yu))))
  (cond (xpos? (cond (ypos? (make-interval (* xl yl) (* xu yu)))
                     (yneg? (make-interval (* xu yl) (* xl yu)))
                     ;; x is strictly positive, but y spans zero, so the lower
                     ;; and upper bounds are the biggest x (xu)) times the
                     ;; corresponding yl/yu
                     (else  (make-interval (* xu yl) (* xu yu)))))
        (xneg? (cond (ypos? (make-interval (* xl yu) (* xu yl)))
                     (yneg? (make-interval (* xu yu) (* xl yl)))
                     (else  (make-interval (* xl yu) (* xl yl))))))
        (else  (cond (ypos? (make-interval (* xl yu) (* xu yu)))
                     (yneg? (make-interval (* xu yl) (* xl yl)))
                     (else  (make-interval (min (* xl yu) (* xu yl))
                                           (max (* xl yl) (* xu yu)))))))
