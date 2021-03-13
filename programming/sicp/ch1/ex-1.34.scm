;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.34

(define (f g) (g 2))

(f f)
(f 2)
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: 2
; [,bt for context]

;; `f` is dfined to apply its argument to 2. Thus:
;; (f f)
;; (f 2) ; first invocation: apply f to 2
;; (2 2) ; second invocation: f's argument is 2, so apply 2 to 2 (nonsensical)
