#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.5

;; Recall cond is handled as:
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
;; Adjust expand-clauses to handle (<test> => <recipient>) actions
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (cond-consequent first)
                     (expand-clauses rest))))))

(define (cond-consequent clause)
  (let ((predicate (cond-predicate clause))
        (actions (cond-actions clause)))
    (if (cond-recipient-clause? clause)
        ((cadr actions) predicate)
        (sequence->exp actions))))

(define (cond-recipient-clause? clause)
  (tagged-list? (cond-actions clause) '=>))

