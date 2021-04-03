;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.24

(list 1 (list 2 (list 3 4)))
;; The translation is list sticks an mcons in front of every argument, and adds
;; the empty list as the cdr of the last argument
(list 3 4)
(mcons 3 (mcons 4 '()))

(list 2 (list 3 4))
(mcons 2 (mcons (mcons 3 (mcons 4 '())) '()))

(list 1 (list 2 (list 3 4)))
(mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))

;; Box-and-pointer representation
;;
;;      ┌───┬───┐    ┌───┬───┐                        
;; ─────▶ ● │   ├────▶ ● │ / │                        
;;      └─┬─┴───┘    └─┬─┴───┘                        
;;        │            │                              
;;      ┌─▼─┐          │                              
;;      │ 1 │        ┌─▼─┬───┐   ┌───┬───┐            
;;      └───┘        │ ● │ ● ├───▶   │ / │            
;;                   └─┬─┴───┘   └─┬─┴───┘            
;;                     │           │                  
;;                   ┌─▼─┐         │                  
;;                   │ 2 │       ┌─▼─┬───┐   ┌───┬───┐
;;                   └───┘       │ ● │ ● ├───▶ ● │ / │
;;                               └─┬─┴───┘   └─┬─┴───┘
;;                                 │           │      
;;                               ┌─▼─┐       ┌─▼─┐    
;;                               │ 3 │       │ 4 │    
;;                               └───┘       └───┘    

;; Tree representation
;;
;;   (1 (2 (3 4)))              
;;  ┌──────●─────┐              
;;  │            │              
;;  │            │              
;;  │            │(2 (3 4))     
;;  │        ┌───●───┐          
;;  1        │       │(3 4)     
;;           │     ┌─●─┐        
;;           2     │   │        
;;                 │   │        
;;                 3   4        
