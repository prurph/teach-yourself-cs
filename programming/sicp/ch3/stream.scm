#lang sicp

;; Trying to use Scheme streams in the Racket REPL is an exercise in
;; frustration. Define these here and use list->stream, not stream, to create
;; stream literals instead of using the Racket streams, which seem really nice
;; but do not play well with the SICP stuff.

(define-syntax cons-stream 
    (syntax-rules ()
        ((cons-stream x y) (cons x (delay y)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l)
                   (list->stream (cdr l)))))

(#%provide stream-car)
(#%provide stream-cdr)
(#%provide the-empty-stream)
(#%provide stream-null?)
(#%provide stream-ref)
(#%provide stream-map)
(#%provide list->stream)
