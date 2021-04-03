;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

;; append expects two lists and returns a single one
(append x y)
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 '())))))))

(display (append x y))
(1 2 3 4 5 6)

;; cons creates a list from two values, here the second value (list 4 5 6) is
;; (cons 4 (cons 5 (cons 6 '()))), so the overall result is:
;; (cons (cons 1 (cons 2 (cons 3 '()))) (cons 4 (cons 5 (cons 6 '()))))
(cons x y)
(cons (cons 1 (cons 2 (cons 3 '())))  ; car is (list 1 2 3)
      (cons 4 (cons 5 (cons 6 '())))) ; cdr is (list 4 5 6)

(display (cons x y))
((1 2 3) 4 5 6)

;; list creates a nil-terminated list from its arguments, so the first element
;; is the list x and the second element is the list y
(list x y)
(cons (cons 1 (cons 2 (cons 3 '())))             ; car is (list 1 2 3)
      (cons (cons 4 (cons 5 (cons 6 '()))) '())) ; cdr is (cons (list 4 5 6) '())

(display (list x y))
((1 2 3) (4 5 6))
