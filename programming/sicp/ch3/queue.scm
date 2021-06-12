#lang sicp

;; Queue
;; Represented as a cons of pointers to the first and last pairs.

;; Select and modify front and rear pointers of a queue.
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; Queue operations
(define (make-queue)
  (cons '() '()))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
;; Return the first _element_ of the queue (that is the car of its first pair).
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    ;; For a previously empty queue, the front and rear point to the new item
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
    ;; For queues with other items, connect the last pair to the new item, and
    ;; adjust the rear pointer.
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))
;; Delete the first item and return the queue. Note this doesn't actually
;; disconnect the first item: its cdr still points to what becomes the new
;; first item. The text has a good visual of this.
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

;; Internals exposed for testing purposes.
(#%provide front-ptr)
(#%provide rear-ptr)
(#%provide set-front-ptr!)
(#%provide set-rear-ptr!)

(#%provide make-queue)
(#%provide empty-queue?)
(#%provide front-queue)
(#%provide insert-queue!)
(#%provide delete-queue!)
