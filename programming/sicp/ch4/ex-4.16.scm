#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.16

;; Issue requiring this from simply-scheme.
(define (filter pred xs)
  (cond ((null? xs) '())
        ((pred (car xs)) (cons (car xs) (filter pred (cdr xs))))
        (else (filter pred (cdr xs)))))

;; 4.16(a)
;; Change lookup-variable-value to signal an error if value is symbol *unassigned*
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop enclosing-environment env))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned symbol: LOOKUP-VARIABLE-VALUE" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: LOOKUP-VARIABLE-VALUE" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

;; 4.16(b)
;; Write a procedure scan-out-defines that takes a procedure body and returns
;; an equivalent one without internal definitions. Go from:
;;
;; (lambda ⟨vars⟩
;;   (define u ⟨e1⟩)
;;   (define v ⟨e2⟩)
;;   ⟨e3⟩)
;;
;; to
;;
;; (lambda ⟨vars⟩
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u ⟨e1⟩)
;;     (set! v ⟨e2⟩)
;;     ⟨e3⟩))
(define (scan-out-defines proc-body)
  (let* ((definitions (filter definition? proc-body))
         (other-exps (filter (lambda (x) (not (definition? x))) proc-body))
         (vars (map definition-variable definitions))
         (vals (map definition-value definitions)))
    ;; let has structure (let (<bindings...>) <exps...>)
    (define (unassigned-binding var)
      (list var '*unassigned*))
    (define (assignment var val)
      (list 'set! var val))
    (append (list 'let (map unassigned-binding vars))
            (map assignment vars vals)
            other-exps)))

;; 4.16(c)
;; Install scan-out-defines
;; Installing it in make-procedure seems preferable because it will do the
;; transformation a single time during evaluation and not each time the
;; procedure is applied.
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
