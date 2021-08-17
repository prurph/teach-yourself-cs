#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.9

;; Support while loops with syntax (while predicate body)

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))
;; Translate to named-let as follows:
;;
;; (let go '()
;;   (if (<while-predicate>)
;;       (begin (<while-body>)
;;              (go))          
;;       #f))
(define (while->named-let exp)
  (list 'let 'go '() (list (make-if (while-predicate exp)
                                    ;; while-body is a list of statements, so
                                    ;; use append; the recursive invocation is
                                    ;; (go) so that's (list 'go), but then we're
                                    ;; appending so make it (list (list 'go)).
                                    (sequence->exp (append (while-body exp)
                                                           (list (list 'go))))
                                    #f))))

;; Add to eval:
;; ((while? exp) (eval (while->named-let exp) env))
