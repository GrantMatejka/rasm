#lang racket

(require "types.rkt")

(require syntax/kerncase)
(require syntax/stx)

(provide build-ast)

(define (build-ast exp)
  (process-top exp))

(define (process-top top-form)
  (kernel-syntax-case top-form #f
    [(module id module-path (#%plain-module-begin module-level-form ...))
     (filter-map (lambda (stx) (process-mod stx)) (stx->list #'(module-level-form ...)))]
    [(#%expression expr) (error 'unsupported)]
    [(begin top-level-form ...) (error 'unsupported)]
    [(begin-for-syntax top-level-form ...) (error 'unsupported)]
    [other (process-gtop top-form)]))

; This handles module and submodule form
(define (process-mod mod-form)
  (kernel-syntax-case mod-form #f
    ; TODO: Build the exports, will need to parse out raw-provide-spec, for now we can just hack all functions to export
    [(#%provide raw-provide-spec ...) #f]
    [(begin-for-syntax module-level-form ...) (error 'unsupported)]
    [(#%declare declaration-keyword ...) (error 'unsupported)]
    ; We won't wory about these forms, they're all submodule forms
    [(module id module-path (#%plain-module-begin module-level-form ...)) #f]
    [(module* id module-path (#%plain-module-begin module-level-form ...)) #f]
    [(module* id #f (#%plain-module-begin module-level-form ...)) #f]
    [other (process-gtop mod-form)]))

; For the general top level forms
(define (process-gtop gtop-form)
  (kernel-syntax-case gtop-form #f
    ; These will all be top level definitions, so either global vars or functions
    [(define-values (id ...) expr)
     (let ((p-ids (filter-map (lambda (stx) (process-formal stx)) (stx->list #'(id ...))))
           (p-expr (process-expr #'expr)))
       (TopDef p-ids p-expr))]
    [(define-syntaxes (id ...) expr) (error 'unsupported)]
    [(#%require raw-require-spec ...) (error 'unsupported)]
    [other (process-expr gtop-form)]))

(define (process-expr expr)
  (kernel-syntax-case expr #f
    [(#%plain-lambda formals expr ...)
     (let ((p-formals (process-formal #'formals))
           (p-exprs (filter-map (lambda (stx) (process-expr stx)) (stx->list #'(expr ...)))))
       (Func p-formals p-exprs))]
    [(case-lambda (formals expr ...) ...) '()]
    [(if test then else) '()]
    [(begin expr ...) '()]
    [(begin0 expr1 expr2 ...) '()]
    [(let-values ([(id ...) val] ...) expr ...) '()]
    [(letrec-values ([(id ...) val] ...) expr ...) '()]
    [(set! id expr) '()]
    [(quote datum) (process-quote #'datum)]
    [(quote-syntax datum) '()]
    [(quote-syntax datum #:local) '()]
    [(with-continuation-mark expr1 expr2 expr3) '()]
    [(#%plain-app expr ...)
     (let ((p-exprs (filter-map (lambda (stx) (process-expr stx)) (stx->list #'(expr ...)))))
       (App (first p-exprs) (rest p-exprs)))]
    [(#%top . id) '()]
    [(#%variable-reference id) '()]
    [(#%variable-reference (#%top . id)) '()]
    [(#%variable-reference) '()]
    [id (process-formal #'id)]
    [other (error 'rasm "Unknown expression " expr)]))

(define (process-formal formal)
  (kernel-syntax-case formal #f
    [(id ...) (map (lambda (i) (Id (syntax-e i))) (stx->list #'(id ...)))]
    [id (Id (syntax-e #'id))]
    ; HELP: What is this??
    [(id1 ... . id2) (error 'unsupported)]
    [other (error 'rasm "Unknown formal " formal)]))

(define (process-quote datum)
  (if (eof-object? (syntax-e datum))
      '()
      (match (syntax-e datum)
        [(? real? r) (Num r)]
        [other (error 'unknown datum)])))

