#lang sicp

;; *** Eval and apply
;; eval takes an expression and environment
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-exps exp) env))
        ((or? exp) (eval-or (or-exps exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let->nested-lets exp) env))
        ((application? exp) (apply (eval (operator exp) env))
                            (list-of-values (operands exp) env))
        (else (error "Unknown expression type: EVAL" exp))))

;; apply takes a procedure and a set of arguments
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; *** Procedure arguments
;; when eval processes a procedure application, it uses list-of-values to
;; produce the list of arguments to which the procedure is to be applied
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; *** Conditionals
;; eval-if evaluates the predicate part of an if expression in the given env
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; *** Sequences
;; eval-sequence is used by:
;; - apply to evaluate the sequence of expressions in a procedure body
;; - eval to evaluate the sequence of expressions in a begin expression
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eal (first-exp exps) env))
        (else (eval (first-exps) env)
              (eval-sequence (rest-exps exps) env))))

;; *** Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; *** Representing Expressions
;; The only self-evaluating items are numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; Variables are represented by symbols
(define (variable? exp) (symbol? exp))

;; tagged-list? identifies lists beginning with a designated symbol
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Quotations have the form (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; Assignments have the form (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Definitions have one of two forms:
;; 1. (define <var> <value>)
;; 2. (define (<var> <parameter₁> ... <parameter₂> ... <parameter_n>)
;;     <body>)
;; The latter form is sugar for:
;;    (define <var>
;;      (lambda (<parameter₁> ... <parameter₂> ... <parameter_n>)
;;        <body>))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)           ; first form: (define <symbol> <value>)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)                ; first form: (define <symbol> <value>)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; lambda expressions are lists that begin with the symbol lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;; constructor for lambda expressions used by definition-value
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; conditionals begin with if and have a predicate, consequent and optional
;; alternative; false is the default alternative
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
;; constructor for if expressions used by cond->if
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin packages a sequence of expressions into a single expression
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cadr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))
;; constructor to transform sequence into a single expression, using begin if
;; necessary, to be used by cond->if
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

;; and/or special forms, which can also be implemented as derived (see ex-4.04.scm)
(define (and? exp) (tagged-list? exp 'and)) 
(define (and-exps exp) (cdr exp))
(define (eval-and exps env)
  (if (null? exps)
      'true
      (if (true? (eval (car exp) env))
          (eval-and (cdr exps) env)
          'false)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exps env)
  (if (null? exps)
      'false
      (if (true? (eval (car exp) env))
          'true
          (eval-or (cdr exps) env))))

;; procedure application is any compound expression that isn't one of the above
;; types
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest ops) (cdr ops))

;; Derived expressions
;; Define in terms of expressions involving other special forms, rather than
;; implement directly. Reduces the number of special forms.

;; evaluate cond as nested if with begin expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-recipient-clause? clause) (tagged-list? (cond-actions clause) '=>))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-consequent clause)
  (let ((predicate (cond-predicate cond-clause))
        (actions (cond-actions cond-clause)))
    (if (cond-recipient-clause? clause)
        ((cadr actions) predicate)
        (sequence->exp actions))))
(define (expand-clauses clauses)
  (if (null? clauses)
      ; value of cond when all predicates false and no else clause unspecified in Scheme--we use false
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first)) ; else can't be a recipient, so shortcut
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (cond-consequent first)
                     (expand-clauses rest))))))

;; evaluate let as lambda
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
;; "Named let" has syntax (let <var> <bindings> <body>).
(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
(define (let->combination exp)
  (if (named-let? exp) ; See ex-4.08 for named-let?
      (sequence->exp (cons (list 'define
                                 (named-let-name exp)
                                 (make-lambda (map car (named-let-bindings exp))
                                              (named-let-body exp)))
                           (cons (named-let-name exp)
                                 (map cadr (named-let-bindings exp)))))
      (cons (make-lambda (map car (let-bindings exp))
                         (let-body exp))
            (map cadr (let-bindings exp)))))
;; evaluate let* as nested lets
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (nest-bindings bindings)
    (if (null? bindings)
        body
        (list 'let (list (car bindings)) (nest-bindings (cdr bindings)))))
  (nest-bindings (let-bindings exp)))

(#%provide eval)
(#%provide apply)
(#%provide list-of-values)
(#%provide eval-if)
(#%provide eval-sequence)
(#%provide eval-assignment)
(#%provide eval-definition)
(#%provide self-evaluating?)
(#%provide variable?)
(#%provide tagged-list?)
(#%provide quoted?)
(#%provide text-of-quotation)
(#%provide assignment?)
(#%provide assignment-variable)
(#%provide assignment-value)
(#%provide definition?)
(#%provide definition-variable)
(#%provide definition-value)
(#%provide lambda?)
(#%provide lambda-parameters)
(#%provide lambda-body)
(#%provide make-lambda)
(#%provide if?)
(#%provide if-predicate)
(#%provide if-consequent)
(#%provide if-alternative)
(#%provide make-if)
(#%provide begin?)
(#%provide begin-actions)
(#%provide last-exp?)
(#%provide first-exp)
(#%provide rest-exp)
(#%provide sequence->exp)
(#%provide and?)
(#%provide and-exps)
(#%provide eval-and)
(#%provide or?)
(#%provide or-exps)
(#%provide eval-or)
(#%provide make-begin)
(#%provide application?)
(#%provide operator)
(#%provide operands)
(#%provide no-operands?)
(#%provide first-operand)
(#%provide rest)
(#%provide cond?)
(#%provide cond-clauses)
(#%provide cond-else-clause?)
(#%provide cond-predicate)
(#%provide cond-actions)
(#%provide cond->if)
(#%provide expand-clauses)
(#%provide let?)
(#%provide let-bindings)
(#%provide let-body)
(#%provide let->combination)
(#%provide named-let?)
(#%provide named-let-name)
(#%provide named-let-bindings)
(#%provide named-let-body)
(#%provide let*?)
(#%provide let*->nested-lets)
