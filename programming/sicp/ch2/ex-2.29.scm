#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; 2.29(a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; 2.29(b)
(define (mobile? structure)
  (pair? structure))

;; A branch-structure is either a primitive number or itself a mobile
(define (total-weight mobile)
  (if (not (mobile? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

;; 2.29(c)
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? mobile)
  (if (not (mobile? mobile))
      #t
      ;; A branch is a length plus a structure. The torque from the left and
      ;; right branches must be equal, and those structures must themselves
      ;; be balanced (simple weight structures are trivially balanced by the
      ;; if clause above).
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (and (equal? (torque lb) (torque rb))
             (balanced? (branch-structure lb))
             (balanced? (branch-structure rb))))))

(define balanced-mobile
    (make-mobile (make-branch 8 2)
                 (make-branch 4 (make-mobile (make-branch 9 1)
                                             (make-branch 3 3)))))

(define unbalanced-mobile
  (make-mobile (make-branch 8 3)
               (make-branch 4 (make-mobile (make-branch 2 1)
                                           (make-branch 2 5)))))

;; 2.29(d)
;; Changed constructors
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; Necessitate the following changes to accessors, because our pairs are no
;; longer null-terminated.
;;   from: (1 2) == (cons 1 (cons 2 '()))
;;   to: (1 . 2) == (cons 1 2)
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))
