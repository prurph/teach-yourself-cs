#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.53

;; memq takes a symbol and a list, returning false if symbol is not in list,
;; otherwise returning the sublist of the list beginning with item

(list 'a 'b 'c)
;; => '(a b c)

(list (list 'george))
;; => (('george))

(cdr '((x1 x2) (y1 y2)))
;; => '((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; => '(y1 y2)

(pair? (car '(a short list)))
;; => #f

(memq 'red '((red shoes) (blue socks)))
;; => #f

(memq 'red '(red shoes blue socks))
;; => '(red shoes blue socks)
