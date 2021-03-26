;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.2

;; point constructor and accessors
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

;; segment constructor and accessors
(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point
  (midpoint-segment
    (make-segment (make-point 0 0)
                  (make-point 10 0)))) ; => (5, 0)

(print-point
  (midpoint-segment
    (make-segment (make-point 0 0)
                  (make-point 0 10)))) ; => (0, 5)

(print-point
  (midpoint-segment
    (make-segment (make-point 1 1)
                  (make-point 11 11)))) ; => (6, 6)
