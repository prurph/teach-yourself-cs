#lang scheme

;; Quick and dirty global table--not designed to be used for different
;; namespaces.
(define table (make-hash))
(define (put op type item)
  (hash-set! table (cons op type) item))
(define (get op type)
  (hash-ref table (cons op type) #f))

;; Representation of stuff as type-tagged contents
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))
;; Apply procedures by looking them up by (op '(type-tag type-tag ...))
;; then applying directly to the contents--the "real" values. These procedures
;; are installed by packages, and are expected to tag their output.
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC" (list op type-tags)))))

;; Provide everything for debugging/investigatory purposes.
(provide table)
(provide put)
(provide get)
(provide attach-tag)
(provide type-tag)
(provide contents)
(provide apply-generic)
