#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.12

;; Use the abstractions make-frame, add-binding-to-frame! and frame-binding
;; Shown with the implementation from Exercise 4.11. Note `assoc` looks
;; up a pair in a list of pairs.
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

;; Now rewriting these in terms of the selectors as we mentioned in 4.11.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (frame-binding var frame)
              (cdr (frame-binding var frame))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (if (frame-binding var frame)
              (set-binding! var val frame)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (frame-binding var frame)
        (set-binding! var val frame)
        (add-binding-to-frame! var val frame))))

(define f (make-frame '(watermelon cherry) '(delicious yum)))
(define env (list f))
(lookup-variable-value 'watermelon env)
(lookup-variable-value 'cherry env)
(define-variable! 'mango 'tasty env)
(set-variable-value! 'watermelon 'delicious! env)
(lookup-variable-value 'watermelon env)
(lookup-variable-value 'mango env)
