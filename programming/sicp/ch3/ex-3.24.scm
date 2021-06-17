#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.24

;; New definition of assoc that compares keys with same-key?
;; Lookup and insert check for keys in (cdr local-table) beacuse the car
;; is our type '*table*. Ditto for subtables; their car is key-1, and their
;; cdr is a list of (key-2 value) pairs.
(define (make-table same-key?)
  (define local-table (list '*table*))
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (define (lookup key-1 key-2)
    (let ((subtable (assoc key-1 (cdr local-table))))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record (cdr record) #f))
          #f)))
  (define (insert! key-1 key-2 value)
    (let ((subtable (assoc key-1 (cdr local-table))))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable (cons (cons key-2 value)
                                         (cdr subtable)))))
          (set-cdr! local-table (cons (list key-1 (cons key-2 value))
                                      (cdr local-table))))
      'ok))
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation: TABLE" m))))
  dispatch)

(define eq-table (make-table eq?))
(define get (eq-table 'lookup-proc))
(define put (eq-table 'insert-proc!))

(put 'ascii 'a 97)
(put 'ascii 'b 98)
(put 'prescott 'favorite-fruit 'watermelon)
(get 'ascii 'a)
(get 'prescott 'favorite-fruit)
