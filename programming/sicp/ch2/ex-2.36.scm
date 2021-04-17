#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.36

(load "accumulate.scm")

;; Given a list of lists all of identical length, accumulate the matching
;; indexes of each sublist, and return a list of these accumulations.
(define (accumulate-n op init seqs)
  ;; Recursing on a lists of lists, so we're done when the first--and therefore
  ;; all because they are the same length--is empty, e.g. ('() '() ...)
  (if (null? (car seqs))
      '()
      ;; Accumulate the first element of each list, then recurse with
      ;; accumulate-n on the remaining portion of each sublists. My initial pass
      ;; had (map (lambda (x) (car x) seqs) but of course this is just creating
      ;; a trivial lambda: just pass `car`!
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))
'(22 26 30)

;; Note how trivial it is to implement zip using accumulate-n!
(define (zip seqs)
  (accumulate-n cons '() seqs))
