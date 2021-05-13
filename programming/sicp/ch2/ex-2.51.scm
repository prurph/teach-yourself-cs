#lang sicp
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.51

(#%require sicp-pict)

;; Below as a separate procedure
(define (below bot-painter top-painter)
  (let ((paint-bot (transform-painter bot-painter
                                      (make-vect 0 0)
                                      (make-vect 1 0)
                                      (make-vect 0 0.5)))
        (paint-top (transform-painter top-painter
                                      (make-vect 0 0.5)
                                      (make-vect 1 0.5)
                                      (make-vect 0 1))))
    (lambda (frame) ((paint-bot frame) (paint-top frame)))))


;; Below as a composite of beside and rotation
(define (below-composite bot-painter top-painter)
  (rotate90 (beside (rotate270 bot-painter) (rotate270 top-painter))))
