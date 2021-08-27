#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.11

;; Represent frame as list of (name value) pair bindings, instead of two lists
;; of (names...) (values...)
(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (if (null? (cdr frame))
        (set-cdr! frame binding)
        (add-binding! binding (cdr frame))))
  (add-binding! (list (cons var val)) frame))

;; These could be written in terms of the selectors above, however the prior
;; versions optimize by knowing about the structure of the frames, so we'll
;; rewrite them also.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (cdar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))
(define (define-variable! var val env)
  (define (scan bindings)
    (cond ((null? bindings)
           (add-binding-to-frame! var val  (first-frame env)))
          ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
          (else (scan (cdr bindings)))))
  (scan (first-frame env)))
