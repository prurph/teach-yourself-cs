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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
         (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

;; Takes elements alternately from two streams to combine them. The alternation
;; is required otherwise if s1 was infinite, no element of s2 would ever appear.
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; Given streams s and t, produce a stream of all possible pairs (list s_x t_y)
;; such that x <= y. The way this breaks down is considering a table above the
;; diagonal:
;;
;;        │                                                           
;;  S0,T0 │ S0,T1  S0,T2 ...                                         
;; ───────┼─────────────────────                                      
;;        │ S1,T1  S1,T2 ...                                         
;;        │                                                           
;;        │        S2,T2 ...                                         
;;        │                                                           
;;
;; This is three parts:
;; 1. S0, T0 (list (stream-car s) (stream-car t))
;; 2. The rest of the first row: S0,T1 S0,T2 ...:
;;    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
;; 3. Recursively pairs formed from (stream-cdr s) and (stream-cdr t)
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s) (stream-cdr t)))))

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l)
                   (list->stream (cdr l)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a
                   (stream-enumerate-interval (+ a 1) b))))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (display-line x) (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-next s n)
  (if (zero? n)
      'done
      (begin (display-line (stream-car s))
             (display-stream-next (stream-cdr s) (- n 1)))))

(#%provide stream-car)
(#%provide stream-cdr)
(#%provide the-empty-stream)
(#%provide stream-null?)
(#%provide stream-ref)
(#%provide stream-map)
(#%provide stream-filter)
(#%provide stream-for-each)
(#%provide add-streams)
(#%provide mul-streams)
(#%provide scale-stream)
(#%provide interleave)
(#%provide pairs)
(#%provide list->stream)
(#%provide stream-enumerate-interval)
(#%provide ones)
(#%provide integers)
(#%provide display-line)
(#%provide display-stream)
(#%provide display-stream-next)
