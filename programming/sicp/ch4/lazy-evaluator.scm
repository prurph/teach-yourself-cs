#lang sicp

;; *** Eval and apply
;; eval takes an expression and environment
;; This is a lazy evaluator; apply is called with operand expressions, rather
;; than the arguments produced by evaluating them. The operator is still
;; evaluated in order to allow `apply` to dispatch on its type (primitive
;; versus compound) and apply it.
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
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp) (my-lazy-apply (actual-value (operator exp) env)
                                           (operands exp)
                                           env))
        (else (error "Unknown expression type: EVAL" exp))))

;; Get the actual value of an expression, forcing it if it is a thunk
(define (actual-value exp env)
  (force-it (eval exp env)))

;; Lazy evaluator creates thunks when procedures are applied to arguments,
;; forcing these thunks later.  A thunk packages an expression together with an
;; environment, so taht the argument can be produced later.
(define (delay-it exp env) (list 'thunk exp env))

;; Represent thunks as a list containing the expression and the environment.
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
;; When a thunk is forced, turn it into an evalauted thunk, replacing the
;; stored expression with its value, and removing the env from the thunk so it
;; can be GC'd.
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '())    ; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;; For this lazy evaluator, `eval` is now passing in unevaluated operand expressions
;; - primitive procedures (strict): evaluate all arguments using
;;   `list-of-arg-values` before applying the primitive
;; - compound procedures (non-strict): delay all arguments before applying the procedure
(define (my-lazy-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            (list-of-delayed-args arguments env)
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; *** Procedure arguments
;; When eval processes a primitive procedure application, it uses
;; list-of-arg-values to produce the list of arguments to which the procedure
;; is to be applied.  For the lazy evaluator, we get the actual-value, forcing
;; any thunks.
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

;; *** Conditionals
;; eval-if evaluates the predicate part of an if expression in the given env
;; Must use actual-value to force the value of the predicate expression before
;; testing it.
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; *** Sequences
;; eval-sequence is used by:
;; - apply to evaluate the sequence of expressions in a procedure body
;; - eval to evaluate the sequence of expressions in a begin expression
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
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
(define (rest-exps seq) (cdr seq))
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
(define (rest-operands ops) (cdr ops))

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
  (let ((predicate (cond-predicate clause))
        (actions (cond-actions clause)))
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
        (let-body exp)
        (list 'let (list (car bindings)) (nest-bindings (cdr bindings)))))
  (nest-bindings (let-bindings exp)))

;; Internal evaluator data structures (Section 4.1.3)
;; Testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Representing procedrues
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on environments Represent an environment as a list of frames. The
;; enclosing environment of an environment is the cdr of the list.
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
;; Each frame of an environment is represented as a pair of lists:
;; 1. List of variables bound in that frame
;; 2. List of associated values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
   (set-car! frame (cons var (car frame)))
   (set-cdr! frame (cons val (cdr frame))))
;; Extend an environment by a new frame, make that frame from lists of vars and
;; vals, and prepend it to the environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error ("Too many arguments supplied" vars vals))
          (error ("Too few arguments supplied" vars vals)))))
;; Lookup variable by scanning frames successively outward
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
;; Set variable by finding it in enclosing frames, then changing its value
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
;; Define variable by changing binding if it exists, otherwise adjoining a
;; binding for it to the first frame.
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;; *** Running the Evaluator as a Program
;; Representing primitive procedures as a list with symbol 'primitive and
;; containing proc in underlying Lisp that implements that primitive
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '* *)
        (list '/ /))) ; add additional primitives as needed
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
;; Primitive procedure application
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))
;; Evaluator reduces expressions ultimately to application of primitive
;; procedrues, so create mechanism to call underlying Lisp system to model
;; application of primitive procedures. Bind primitive procedures in a global
;; environment.

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
;; Driver loop that models REPL
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
;; Avoid printting environment portion of compound procdure; may be large or
;; contain cycles.
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;; Run evaluator by initializing global env and starting driver loop:
(define (run-evaluator)
  ;;  that the-global-environment is already defined
  (driver-loop))

(#%provide eval)
(#%provide actual-value)
(#%provide force-it)
(#%provide delay-it)
(#%provide force-it)
(#%provide delay-it)
(#%provide thunk?)
(#%provide thunk-exp)
(#%provide thunk-env)
(#%provide my-lazy-apply)
(#%provide list-of-arg-values)
(#%provide list-of-delayed-args)
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
(#%provide rest-exps)
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
(#%provide rest-operands)
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
(#%provide true?)
(#%provide false?)
(#%provide make-procedure)
(#%provide compound-procedure?)
(#%provide procedure-parameters)
(#%provide procedure-body)
(#%provide procedure-environment)
(#%provide enclosing-environment)
(#%provide first-frame)
(#%provide the-empty-environment)
(#%provide make-frame)
(#%provide frame-variables)
(#%provide frame-values)
(#%provide add-binding-to-frame!)
(#%provide extend-environment)
(#%provide lookup-variable-value)
(#%provide set-variable-value!)
(#%provide define-variable!)
(#%provide setup-environment)
(#%provide the-global-environment)
(#%provide primitive-procedure?)
(#%provide primitive-implementation)
(#%provide primitive-procedures)
(#%provide primitive-procedure-names)
(#%provide primitive-procedure-objects)
(#%provide apply-primitive-procedure)
(#%provide driver-loop)
(#%provide user-print)
(#%provide run-evaluator)
