#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.44

(#%require sicp-pict)

;; Note that either I'm an idiot and somehow misreading them, or the sicp-pict
;; docs are incorrect. They say of `below`: "Constructs a painter that paints
;; the second image below the first", but (below p1 p2) results in p1 being
;; below p2. To my mind that means it paints the first image below the second.
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 2))
