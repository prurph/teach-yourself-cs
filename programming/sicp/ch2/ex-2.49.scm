#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.49

(#%require sicp-pict)

;; Given the following, which exists in sicp-pict), define:
(define (segments->painter segment-list)
  (lambda (frame) (for-each (lambda (segment)
                              (draw-line ((frame-coord-map frame) (start-segment segment))
                                         ((frame-coord-map frame) (end-segment segment))))
                            segment-list)))

;; 2.49(a) The painter that draws the outline of the designated frame. Recall
;; segments->painter takes a list of segments relative to the unit frame,
;; because that's what we draw in.
(define paint-outline
  (let ((origin (make-vect 0 0))
        (lr (make-vect 1 0))
        (tl (make-vect 0 1))
        (tr (make-vect 1 1)))
  (segments->painter (list (make-segment origin lr)
                           (make-segment origin tl)
                           (make-segment lr tr)
                           (make-segment tl tr)))))

;; 2.49(b) The painter that draws an X by connecting opposite corners of the
;; frame
(define paint-x
  (let ((origin (make-vect 0 0))
        (lr (make-vect 1 0))
        (tl (make-vect 0 1))
        (tr (make-vect 1 1)))
  (segments->painter (list (make-segment origin tr)
                           (make-segment tl lr)))))

;; 2.49(c) The painter that draws a diamond shape by connecting the midpoints of
;; the sides of the frame.
(define paint-diamond
  (let ((bmid (make-vect 0.5 0))
        (lmid (make-vect 0 0.5))
        (tmid (make-vect 0.5 1))
        (rmid (make-vect 1 0.5)))
  (segments->painter (list (make-segment bmid lmid)
                           (make-segment bmid rmid)
                           (make-segment lmid tmid)
                           (make-segment rmid tmid)))))

;; 2.49(d) No thanks on the wave painter!
