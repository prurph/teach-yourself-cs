#lang sicp

(#%require "evaluator.scm")

;; An evalator that performs syntactic analysis only once (Ch 4.1.7)
(define (eval exp env) ((analyze exp) env))

;; analyze performs syntactic analysis and returns a new prodcedure, the
;; *execution procedure*, that encapsulates the work to be done in executing
;; the analyzed expression. Eval then applies this to the environment.
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))
;; variable lookup must be done during execution (depends on knowing
;; environment)
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze definition-value exp)))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))
;; Analyze predicate, consequent, and alternative at analysis time
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))
;; Analyze lambda body only once, even though it may be applied multiple times
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))
;; Analyze each expression in the sequence, yielding an execution procedure.
;; Then create a single execution procedure that appplies the enviornment
;; sequentially to each individually execution procedure
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))
;; Analyze an application by analyzing operator and operands and construct an
;; execution procedure that calls operator execution procedure (to obtain the
;; actual procedure to be applied) and the operand execution procedures (to
;; obtain the actual arguments)
(define (analyze-application exp)
  (let ((fproc (analyze operator exp))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env)) aprocs)))))
;; execute-application is the analog of `apply` in evaluator.scm. In this case
;; the procedure body for a compound procedure has already been analyzed, so no
;; further analysis is required. Instead, just call the execution procedure for
;; the body on the extended environment.
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ;; Extend the environment by our variables (parameters to this procedure)
         ;; and their values (args)
         ((procedure-body proc) (extend-environment (procedure-parameters proc)
                                                    args
                                                    (procedure-environment proc))))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

(#%provide eval)
(#%provide execute-application)
