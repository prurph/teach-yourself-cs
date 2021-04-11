#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.25

;; Extract 7 from various lists
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))
(car (cdaddr l)) ; Built-ins exist for c[ad]{1,4}r

(define l (list (list 7)))
;; This is (mcons (mcons 7 '()) '())
(car (car l))
(caar l)

(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; Each (car (cdr  ... goes down one level. The cdr part moves to the nested
;; list, then the car gets the actual "list" part instead of '(). Recall that
;;   (list 1 (list 2))                 is
;;   (cons 1 (cons (list 2) '())       which is
;;   (cons 1 (cons (cons 2 '()) '()))
;; So we can deconstruct:
;;   (car (cdr (list 1 (list 2))))
;;   (car (cons (list 2) '()))
;;   (list 2)
;;   (cons 2 '())
;; From above, 5 (car (cdr invocations get to (list 6 7) and then from that we
;; take the cdr to get (cons 7 '()) and then the car to get 7. Overall, 6
;; iterations of (car (cdr
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

