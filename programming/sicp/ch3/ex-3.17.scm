#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.17

;; Need to do this by returning a lambda--or using an internal helper fucntion--
;; in order to have a closure over already-seen. If we just do:
;;   (define already-seen '())
;;   (if ...
;; then the recursive calls to count-pairs will have already-seen shadowed by
;; new empty lists and nothing is effectively ever already-seen.
(define count-pairs
  (let ((already-seen '()))
    (lambda (x)
      (if (or (not (pair? x))
              (memq x already-seen))
          0
          ;; Include this pair in already-seen first. It is now being counted by the
          ;; +1 as the third argument here.
          (begin (set! already-seen (cons x already-seen))
                 (+ (count-pairs (car x))
                    (count-pairs (cdr x))
                    1))))))

;; All of these should now correctly return 3
(define count-3 (list 1 2 3))

(define count-4
  (let ((l1 (list 4)))
    (cons 1 (cons l1 l1))))

(define count-7
  (let* ((l1 (list 7))
         (l2 (cons l1 l1)))
    (cons l2 l2)))

(define count-inf
  (let ((l1 (list 'c 'a 't)))
    (set-cdr! (cdddr l1) l1)
    l1))

(count-pairs count-3)
(count-pairs count-4)
(count-pairs count-7)
(count-pairs count-inf)
