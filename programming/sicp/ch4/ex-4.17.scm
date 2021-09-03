#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.17

;; When definitions are scanned out, there is an extra frame because the scanning process creates a let expression, which is then evaluated as a lambda, causing the creation of another enviornment frame. This cannot affect the interpretation of the program because the new--nested, inner frame--insulates the let definitions from the outer environment.
;;
;; To avoid creating the extra frame, all internal definitions must occur before the body of the outer procedure is evaluated. Otherwise we have no guarantee that the internal defines would be correctly bound in the environment used to evaluate the procedure body. When we scan them out the extra frame takes care of this by setting any yet-undefined references to '*unassigned*, allowing them to be filled in later.
