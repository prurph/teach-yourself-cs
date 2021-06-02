#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.84

;; By using a generic type-level method, we can add new type-levels easily
;; simply by adding them to the table with `put` as below.
(define (type-level type-tag)
  (apply-generic 'type-level (list type-tag)))

(put 'type-level '(integer) (lambda (x) 0))
(put 'type-level '(rational) (lambda (x) 1))
(put 'type-level '(real) (lambda (x) 2))

;; Note there is a `raise` method defined in racket, but we'll assume this raise
;; is the one from ex-2.83 that just calls apply-generic 'raise 
(define (apply-generic op . args)
  (define (raise-to arg type-level)
    (cond ((= (type-level arg) type-level) arg)
          ((< (type-level arg) type-level) (raise-to (raise arg) type-level))
          (else (error "Cannot raise to a lower target level" arg type-level))))
  (define (raise-apply type-tags)
    (let ((max-type-level (apply max (map type-level type-tags))))
      ;; Note that just like ex-2.83 we apply apply-generic here because there
      ;; is a list of args, and apply-generic is defined as taking varargs, not
      ;; a list of arguments.
      (apply apply-generic
             op
             (map (lambda (x) (raise-to x max-type-level))
                  args))))
  (let ((type-tags (map type-tag args))
        (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (raise-apply type-tags))))
