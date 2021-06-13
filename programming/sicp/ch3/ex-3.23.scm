#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.23


;; This file can be loaded as a module: (require "ex-2.23.scm").

(define (make-deque)
  (define front '())
  (define rear '())
  ;; Internal representation of a node as (val (prev . next))
  (define (make-deque-node val prev next)
    (cons val (cons prev next)))
  (define (empty-deque?)
    (null? front))
  (define (front-deque)
    (if (empty-deque?)
        (error "FRONT called with an empty deque" (print))
        (car front)))
  (define (rear-deque)
    (if (empty-deque?)
        (error "REAR called with an empty deque" (print))
        (car rear)))
  (define (front-insert-deque! item)
    (cond ((empty-deque?)
           (let ((new-node (make-deque-node item '() '())))
             (set! front new-node)
             (set! rear new-node)
             dispatch))
          (else
            (let ((new-node (make-deque-node item '() front)))
              (set-car! (cdr front) new-node)
              (set! front new-node)
              dispatch))))
  (define (rear-insert-deque! item)
    (cond ((empty-deque?)
           (front-insert-deque! item))
          (else
            (let ((new-node (make-deque-node item rear '())))
              (set-cdr! (cdr rear) new-node)
              (set! rear new-node)
              dispatch))))
  (define (front-delete-deque!)
    (cond ((empty-deque?)
           (error "FRONT-DELETE! called with an empty deque" (values)))
          (else
            ;; cddr of a node is its next pointer
            (set! front (cddr front))
            (if (empty-deque?)
                (set! rear '())
                ;; Remove the prev pointer from the new front of the deque
                (set-car! (cdr front) '()))
            dispatch)))
  (define (rear-delete-deque!)
    (cond ((empty-deque?)
           (error "REAR-DELETE! called with an empty deque" (values)))
          (else
            ;; cadr of a node is its prev pointer
            (set! rear (cadr rear))
            (if (null? rear)
                (set! front '())
                ;; Remove the next pointer from the new rear of the deque
                (set-cdr! (cdr rear) '())))))
  ;; Internal method to collect all values
  (define (values)
    ;; Start from the rear node and follow prev pointers back toward the head.
    ;; Go in reverse so we have constant time cons prepend operations.
    (define (iter node acc)
      (if (null? node)
          acc
          (iter (cadr node) (cons (car node) acc))))
    (iter rear '()))
  (define (print)
    (display (values)))
  (define (dispatch m)
    (cond ((eq? m 'empty-deque?) (empty-deque?))
          ((eq? m 'front-deque) (front-deque))
          ((eq? m 'rear-deque) (rear-deque))
          ((eq? m 'front-insert-deque!) front-insert-deque!)
          ((eq? m 'rear-insert-deque!) rear-insert-deque!)
          ((eq? m 'front-delete-deque!) (front-delete-deque!))
          ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
          ((eq? m 'values) (values))
          ((eq? m 'print) (print))
          (else (error "MAKE-DEQUE unknown action" m))))
  dispatch)

(define (empty-deque? d) (d 'empty-deque?))
(define (front-deque d) (d 'front-deque))
(define (rear-deque d) (d 'rear-deque))
(define (front-insert-deque! d item) ((d 'front-insert-deque!) item))
(define (rear-insert-deque! d item) ((d 'rear-insert-deque!) item))
(define (front-delete-deque! d) (d 'front-delete-deque!))
(define (rear-delete-deque! d) (d 'rear-delete-deque!))
(define (values-deque d) (d 'values))
(define (print-deque d) (d 'print))

(#%provide make-deque)
(#%provide empty-deque?)
(#%provide front-deque)
(#%provide rear-deque)
(#%provide front-insert-deque!)
(#%provide rear-insert-deque!)
(#%provide front-delete-deque!)
(#%provide rear-delete-deque!)
(#%provide values-deque)
(#%provide print-deque)
