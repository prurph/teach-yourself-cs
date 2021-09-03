#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.18

;; Expanding the following with each definition scanning out strategy:
;;
;; (define (solve f y0 dt)
;;   (define y (integral (delay dy) y0 dt))
;;   (define dy (stream-map f y))
;;   y)
;;
;; For the single let in the text:
(lambda (f y0 dt)
  (let ((y '*unassigned*) (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
;; This works properly, y and dy are both set and then y is returned. Even though y and dy are values  and not procedures, as  the evaluation of dy is delayed, when y is evaluated in the procedure body, dy will not be undefined. This is effectively wrapping dy in a lambda, and "converting" it into a procedure. When dy is set y is no longer undefined, so that part is fine.

;; For the nested let with renames:
(lambda (f y0 dt)
  (let ((y '*unassigned*) (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))
;; This doesn't work, b is assigned to (stream-map f y), and at that point y is '*unassigned*.
