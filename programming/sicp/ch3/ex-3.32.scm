#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.32

;; For an and-gate defined as:
;;
;;   (define (and-gate a1 a2 output) 
;;     (define (and-action-procedure)
;;       (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
;;         (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
;;     (add-action! a1 and-action-procedure)
;;     (add-action! a2 and-action-procedure)
;;     'ok)
;; 
;; Assume (a1, a2) changes from (0, 1) to (1, 0) at the same agenda time. This
;; is equivalent to calling:
;;
;;   (set-signal! a1 1)
;;   (set-signal! a2 0)
;;
;; Setting the signals causes each wire's action procedures to be run, in turn
;; adding items to the agenda via `after-delay`. Since the delay is the same,
;; both procedures will be assigned to the same time segment.
;; Given the order of the above signal setting, a1 will add its procedure to
;; that time segment with (a1, a2) as (1, 1), because a2 hasn't changed yet;
;; a2 will add then its procedure to the segment with the final value of (1,
;; 0).
;;
;; Thus if the segment uses a queue, a1's procedure runs first, and sets the
;; output to 1 (because it sees (1, 1) without a2's updated value), but
;; immediately thereafter a2's procedure runs and overwrites the value with the
;; correct output of 0, so the overall output for the and gate at this time
;; segment is 0.
;;
;; If instead the segment uses LIFO semantics, a2's procedure runs first, but
;; its value of 0 is then overwritten with the incorrect value of 1 from a2's
;; procedure.
