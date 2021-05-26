#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-17.html#%_thm_2.76

;; Changes required to add new types or new operations
;;
;; 1. Generic Operations With Explicit Dispatch
;;   - New types
;;     - Write procedures for each generic operation for new type, and
;;       constructors for it
;;     - All generic operations must be updated to handle the new type and
;;       dispatch to the appropriate procedure
;;   - New operations
;;     - Implement procuedures for new operation on all types
;;     - Implement generic operation that calls the above procedures
;; 2. Data-Directed Style (procedures stored under tagged table by op and type)
;;   - New types
;;     - Write procedures to perform operations on the new type and constructors
;;     - Install procedures in table
;;   - New operations
;;     - Implement procedures to perform this operation on all types
;;     - Install those procedures in the table
;; 3. Message-Passing
;;   - New types
;;     - Create a constructor for a new dispatch object that responds to the
;;       existing messages (operations)
;;   - New operations
;;     - Edit all existing dispatch procedures to add the new operation

;; If frequently adding new types, message-passing or data-directed with
;; modules grouped by procedures on a given type are ideal.
;;
;; If frequently adding new operations, data-directed with modules factored by
;; operation (that is, a module provides an operation for various types) is
;; preferable to message-passing.
