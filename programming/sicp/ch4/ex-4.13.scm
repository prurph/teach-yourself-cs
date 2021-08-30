#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.13

;; Implement make-unbound!, which unbinds a variable in the first frame of an
;; environment. Rather than remove the binding in _any_ frame of the
;; environment, these semantics are  chosen to make nested environments act as
;; pass by value instead of reference, and preserve lexical scope.
;;
;; Uses the following from Exercise 4.12:
(define (make-frame variables values)
  (map cons variables values)) 
(define (frame-binding var frame)
  (assoc var frame))
(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (if (null? (cdr frame))
        (set-cdr! frame binding)
        (add-binding! binding (cdr frame))))
  (add-binding! (list (cons var val)) frame))
(define (set-binding! var val frame)
  (set-cdr! (frame-binding var frame) val))

;; Could alternatively use `frame-binding` and then set its car (the
;; variable identifier) to null, however that would cause the frame
;; to grow as values were defined and unbound.
(define (make-unbound! var env)
  (define (scan bindings prev)
    (cond ((null? bindings) (error "Unbound variable: MAKE-UNBOUND!" var))
          ((eq? var (caar bindings))
           (if (null? prev)
               ;; Need a new head: set car to the car of the rest
               ;; set the rest to the cdr of the rest.
               (begin (set-car! bindings (cadr bindings))
                      (set-cdr! bindings (cddr bindings)))
               (set-cdr! prev (cdr bindings))))
          (else (scan (cdr bindings) bindings))))
  (scan (first-frame env) '()))

;; Alternate implementation as described above.
(define (make-unbound-with-null! var env)
  (define (scan frame)
    (if (frame-binding var frame)
        (set-car! (frame-binding var frame) '())
        (error  "Unbound variable: MAKE-UNBOUND-WITH-NULL!" var)))
  (scan (first-frame env)))

;; Test cases
;;
;; Remove tail twice
(define f (make-frame '(watermelon cherry mango) '(delicious yum hooray)))
(define env (list f))
(make-unbound! 'mango env)
(make-unbound! 'cherry env)

;; Remove head twice
(define f (make-frame '(watermelon cherry mango) '(delicious yum hooray)))
(define env (list f))
(make-unbound! 'watermelon env)
(make-unbound! 'cherry env)

;; Remove head then tail
(define f (make-frame '(watermelon cherry mango) '(delicious yum hooray)))
(define env (list f))
(make-unbound! 'watermelon env)
(make-unbound! 'mango env)

;; Remove internal element
(define f (make-frame '(watermelon cherry mango) '(delicious yum hooray)))
(define env (list f))
(make-unbound! 'cherry env)
