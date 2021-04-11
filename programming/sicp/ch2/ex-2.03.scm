#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.3

;; point from ex-2.02
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (distance-point p1 p2)
  (sqrt (+ (sqr (- (x-point p1) (x-point p2)))
           (sqr (- (y-point p1) (y-point p2))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; The area and perimeter of a rectangle do not depend on its representation,
;; only its width and height properties.
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

;; One possible rectangle representation: origin, rotation, width, height
(define (make-rect width height origin rotation)
  (cons (cons width height)
        (cons origin rotation))))
(define (width-rect r)
  (car (car r)))
(define (height-rect r)
  (cdr (car r)))
(define (origin-rect r)
  (car (cdr r)))
(define (rotation-rect r)
  (cdr (cdr r)))

;; Another possible rectangle representation: 3 points
;; Relearned vector math/dot products from the excellent https://codology.net/post/sicp-solution-exercise-2-3/
;; Dot-product and orthogonality assumes points that represent vectors
;; originating at the--wait for it--origin.
(define (dot-product p1 p2)
  (+ (* (x-point p1) (x-point p2))
     (* (y-point p1) (y-point p2))))
(define (orthogonal? v1 v2)
  (zero? (dot-product v1 v2)))

(define (make-rect p1 p2 p3)
  (if (orthogonal? (make-point (- (x-point p1) (x-point p2))
                               (- (y-point p1) (y-point p2)))
                   (make-point (- (x-point p1) (x-point p3))
                               (- (y-point p1) (y-point p3))))
      (cons p1 (cons p2 p3))
      (error "Points must form a rectangle")))
(define (p1-rect r)
  (car r))
(define (p2-rect r)
  (car (cdr r)))
(define (p3-rect r)
  (cdr (cdr r)))
;; height and width accessors allow area-rect and perimeter-rect to work with
;; this representation as well
(define (height-rect r)
  (distance-point (p1-rect r) (p2-rect r)))
(define (width-rect r)
  (distance-point (p1-rect r) (p3-rect r)))

