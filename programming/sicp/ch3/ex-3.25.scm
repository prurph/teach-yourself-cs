#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.25

;; Slightly different representation than the book: instead of actual pairs for
;; key-values, we use (list <key> <value> (list <nested_key> <nested_value>).
;; Always write the value as #f when creating a subtable.
(define (make-table)
  (define local-table (list '*table*))
  (define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (define (lookup keys table)
    (cond ((null? keys) (car table))
          (else (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (lookup (cdr keys) (cdr subtable))
                      #f)))))
  (define (insert! keys value table)
    (cond ((null? keys) (set-car! table value))
          (else (let ((subtable (assoc (car keys) (cdr table))))
                  (cond ((not subtable)
                         (set-cdr! table (cons (list (car keys) #f)
                                               (cdr table)))
                         (set! subtable (cadr table))))
                  (insert! (cdr keys) value (cdr subtable)))))
    'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
          ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
          ((eq? m 'display) (display local-table))
          ((eq? m 'debug) local-table)
          (else (error "Unknown operation: TABLE" m))))
  dispatch)

(define table (make-table))
(define get (table 'lookp-proc))
(define put (table 'insert-proc!))

(put (list 'ascii 'a) 97)
(put (list 'ascii 'b) 98)
(put (list 'prescott 'favorite-fruit) 'watermelon)
(get (list 'ascii 'a))
(get (list 'prescott 'favorite-fruit))
