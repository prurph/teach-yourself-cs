#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.21

(#%require "queue.scm")

(define q1 (make-queue))
(insert-queue! q1 'a)
;; => ((a) a)
(insert-queue! q1 'b)
;; => ((a b) b)
(delete-queue! q1)
;; => ((b) b)
(delete-queue! q1)
;; => (() b)

;; The reason these return values look incorrect is that the queue itself, q1,
;; is a cons of head and tail pointers, it is _NOT_ a list of the elements in
;; the queue. Thus:
;; 
;; (insert-queue! q1 'a)
;; - car of queue (front-ptr) is now the first pair (cons a '()) → (a)
;; - cdr of queue (rear-ptr) is also the first pair, which is displayed as just
;;   a since its part of the list
;; (insert-queue! q1 'b)
;; - car of queue (front-ptr) still points to first pair, but now that is
;;   (cons a (cons b '()))
;; - cdr of queue (rear-ptr) is b
;;
;; Similar logic explains the other results. In particular the last delete that
;; empties the queue is an artifact of delete not clearing the rear-ptr if it
;; empties the list. This is okay--as explained in the text--because any calls
;; to `empty-queue?` still correctly return true given that it looks only at the
;; front pointer.

(define (print-queue queue)
  (display (front-ptr queue)))
