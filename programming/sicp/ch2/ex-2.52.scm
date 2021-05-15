#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.52

(#%require sicp-pict)

;; original wave from http://community.schemewiki.org/?sicp-ex-2.49
;; 2.52(a) added segments to add a beak
(define wave-mod (segments->painter (list (make-segment (make-vect .25 0)
                                                        (make-vect .35 .5))
                                          (make-segment (make-vect .35 .5)
                                                        (make-vect .3 .6))
                                          (make-segment (make-vect .3 .6)
                                                        (make-vect .15 .4))
                                          (make-segment (make-vect .15 .4)
                                                        (make-vect 0 .65))
                                          (make-segment (make-vect 0 .65)
                                                        (make-vect 0 .85))
                                          (make-segment (make-vect 0 .85)
                                                        (make-vect .15 .6))
                                          (make-segment (make-vect .15 .6)
                                                        (make-vect .3 .65))
                                          (make-segment (make-vect .3 .65)
                                                        (make-vect .4 .65))
                                          (make-segment (make-vect .4 .65)
                                                        (make-vect .35 .85))
                                          (make-segment (make-vect .35 .85)
                                                        (make-vect .4 1))
                                          (make-segment (make-vect .4 1)
                                                        (make-vect .6 1))
                                          (make-segment (make-vect .6 1)
                                                        (make-vect .65 .85))
                                          (make-segment (make-vect .65 .85)
                                                        (make-vect .6 .65))
                                          (make-segment (make-vect .6 .65)
                                                        (make-vect .75 .65))
                                          (make-segment (make-vect .75 .65)
                                                        (make-vect 1 .35))
                                          (make-segment (make-vect 1 .35)
                                                        (make-vect 1 .15))
                                          (make-segment (make-vect 1 .15)
                                                        (make-vect .6 .45))
                                          (make-segment (make-vect .6 .45)
                                                        (make-vect .75 0))
                                          (make-segment (make-vect .75 0)
                                                        (make-vect .6 0))
                                          (make-segment (make-vect .6 0)
                                                        (make-vect .5 .3))
                                          (make-segment (make-vect .5 .3)
                                                        (make-vect .4 0))
                                          (make-segment (make-vect .4 0)
                                                        (make-vect .25 0))
                                          (make-segment (make-vect .48 .75)
                                                        (make-vect .50 .72))
                                          (make-segment (make-vect .50 .72)
                                                        (make-vect .52 .75)))))

;; 2.52(b) altered corner-split pattern (requires up-split and right-split)
(define (right-split painter n)
  (if (zero? n)
      painter
      (beside painter (below (right-split painter (- n 1))
                             (right-split painter (- n 1))))))

(define (up-split painter n)
  (if (zero? n)
      painter
      (below painter (beside (up-split painter (- n 1))
                             (up-split painter (- n 1))))))

(define (corner-split-mod painter n)
  (if (zero? n)
      painter
      (beside (below painter
                     (up-split painter (- n 1)))
              (below (right-split painter (- n 1))
                     (corner-split-mod painter (- n 1))))))

;; 2.52(c) modified square-limit to make einstein look inward toward the center
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter))
           (beside (tl painter) (tr painter)))))

;; Original corner-split
(define (corner-split painter n)
  (if (zero? n)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; Einstein faces right by default, so the identity painter should be in the
;; bottom-left
(define (square-limit-mod painter n)
  (if (zero? n)
      painter
      ((square-of-four flip-vert rotate180
                       (lambda (x) x) flip-horiz) (corner-split painter n))))
