#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-z-h-15.html#%_thm_2.46

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

;; Another clever implementation is (add-vect v1 (scale-vect -1 v2)), courtesy
;; of Drew Hess at http://wiki.drewhess.com/wiki/sicp_exercise_2.46.
(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))
