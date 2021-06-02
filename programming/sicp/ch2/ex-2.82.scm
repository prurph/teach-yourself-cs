#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.82

(require "generic-arithmetic.scm")

(define type-tag "hi")
(define get-coercion identity)

(define (apply-generic op . args)
  ;; Return the coercion proc to `coerce` arg to `coerce-to`.
  ;; - Returns the identity if arg is already the desired type
  ;; - Returns #f if no such coercion procedure exists
  (define (coercion-for-arg arg coerce-to)
    (if (equal? (type-tag arg) coerce-to)
        identity
        (get-coercion (type-tag arg) coerce-to)))
  ;; Try to coerce all args into each tag in `type-tags` in turn, and if
  ;; possible, apply op to those converted args and return it.
  (define (try-coercive-apply type-tags)
    (if (null? type-tags)
        (error "No method for these types" (list op type-tags))
        (let* ((coerce-to (car type-tags))
               (coercions (map (lambda (x) (coercion-for-arg x coerce-to))
                               args)))
          ;; Coercion is possible for all types, use the fancy (map fn . lists)
          ;; syntax to apply each coercion to its arg. This is basically zip
          ;; and then reduce by applying. Could also do something funky like:
          ;;   (apply-generic op (map apply coercions (map list args))
          ;; but the lambda is a little easier to read I think.
          ;; Also every in the simply-scheme package maps to sent-every so it's
          ;; better to avoid using it.
          (if (not (member #f coercions))
              (apply-generic op (map (lambda (coercion arg) (coercion arg))
                                     coercions
                                     args))
              (try-coercive-apply (cdr type-tags))))))
  (let* ((type-tags (map type-tag args))
         (proc (get op-type-tags)))
    (if proc
        (apply proc (map contents args))
        (try-coercive-apply type-tags))))

