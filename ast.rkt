#lang racket

(require "types.rkt")

(require syntax/kerncase
         syntax/stx)

(provide build-ast)

; TODO: Get list libraries compiling

; Datatypes of interest: string, pair, list

(define (build-ast exp)
  (let ((ast (process-top exp)))
    (let ((vardefs (filter-map (lambda (td) (if (Func? (TopDef-val td)) #f (VarDef (TopDef-ids td) (TopDef-val td)))) ast))
          (funcdefs (append (filter-map (lambda (td) (if (Func? (TopDef-val td)) (FuncDef (TopDef-ids td) (TopDef-val td)) #f)) ast)
                            (hash-map LAMBDAS (lambda (k v) (FuncDef (Id k) v))))))
      (hash-clear! LAMBDAS)
      (Program vardefs funcdefs))))

(define LAMBDAS (make-hash))

(define (process-top top-form)
  (kernel-syntax-case top-form #f
    [(module id module-path (#%plain-module-begin module-level-form ...))
     (filter-map (lambda (stx) (process-mod stx)) (stx->list #'(module-level-form ...)))]
    ; WILLDO?
    [(#%expression expr) (error 'unsupported)]
    [(begin top-level-form ...) (error 'unsupported)]
    [(begin-for-syntax top-level-form ...) (error 'unsupported)]
    [other (process-gtop top-form)]))

; This handles module and submodule form
(define (process-mod mod-form)
  (kernel-syntax-case mod-form #f
    ; TODO: Build the exports, will need to parse out raw-provide-spec, for now we can just hack all functions to export
    [(#%provide raw-provide-spec ...) #f]
    ; WILLDO?
    [(begin-for-syntax module-level-form ...) (error 'unsupported)]
    ; WILLDO? This doesn't matter?
    [(#%declare declaration-keyword ...) #f]
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
           (p-expr (process-expr #'expr #t)))
       ; TODO: Actually handle multiple return vals
       (TopDef (first p-ids) p-expr))]
    ; WILLDO?
    [(define-syntaxes (id ...) expr) (error 'unsupported)]
    [(#%require raw-require-spec ...) (error 'unsupported)]
    [other (process-expr gtop-form)]))

(define (process-expr expr [top-level? #f])
  (kernel-syntax-case expr #f
    [(#%plain-lambda formals expr ...)
     (let ((p-formals (process-formal #'formals))
           (p-exprs (filter-map (lambda (stx) (process-expr stx)) (stx->list #'(expr ...)))))
       (if top-level?
           (Func p-formals p-exprs)
           (let ((id (gensym 'lambda)))
             (hash-set! LAMBDAS id (Func p-formals p-exprs))
             (Id id))))]
    [(if test then else)
     (let ((p-test (process-expr #'test))
           (p-then (process-expr #'then))
           (p-else (process-expr #'else)))
       (If p-test p-then p-else))]
    [(begin exprs ...)
     (let ((p-exprs (filter-map process-expr (stx->list #'(exprs ...)))))
       (Begin p-exprs))]
    ; TODO: This technically isn't the correct form, but we need to figure out how to handle multiple return values
    [(let-values ([ids vals] ...) exprs ...)
     (let ((p-ids (filter-map process-expr (stx->list #'(ids ...))))
           (p-vals (filter-map process-expr (stx->list #'(vals ...))))
           (p-exprs (filter-map process-expr (stx->list #'(exprs ...)))))
       (LetVals p-ids p-vals p-exprs))]
    [(#%plain-app expr ...)
     (let ((p-exprs (filter-map process-expr (stx->list #'(expr ...)))))
       (App (first p-exprs) (rest p-exprs)))]
    [(quote datum) (process-quote #'datum)]
    [id (process-formal #'id)]
    ; WILLDO?
    [(begin0 expr1 expr2 ...) '()]
    [(case-lambda (formals expr ...) ...) '()]
    [(letrec-values ([(id ...) val] ...) expr ...) '()]
    [(set! id expr) '()]
    [(quote-syntax datum) '()]
    [(quote-syntax datum #:local) '()]
    [(with-continuation-mark expr1 expr2 expr3) '()]
    [(#%top . id) '()]
    [(#%variable-reference id) '()]
    [(#%variable-reference (#%top . id)) '()]
    [(#%variable-reference) '()]
    [other (error 'rasm "Unknown expression " expr)]))

(define (process-formal formal)
  (kernel-syntax-case formal #f
    [(id ...) (map (lambda (i) (Id (syntax-e i))) (stx->list #'(id ...)))]
    [id (Id (syntax-e #'id))]
    ; HELP: What is this??
    ; WILLDO?
    [(id1 ... . id2) (error 'unsupported)]
    [other (error 'rasm "Unknown formal " formal)]))

(define (process-quote datum)
  (if (eof-object? (syntax-e datum))
      '()
      (match (syntax-e datum)
        [(? integer? r) (Int r)]
        [(? real? r) (Float r)]
        [(? boolean? b) (if b (Int 1) (Int 0))]
        [other (error 'unknown (~a datum))])))


