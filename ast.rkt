#lang racket

(require "types.rkt")

(require syntax/kerncase
         syntax/stx)

(provide build-ast)

(define TOP_LVL_FORM_ERROR "ERROR: Please provide a module as the top form to compile")


(define (build-ast exp)
  (let ((ast (process-top exp)))
    (let ((vardefs (filter-map (lambda (td) (if (Func? (TopDef-val td)) #f (VarDef (TopDef-id td) (TopDef-val td)))) ast))
          (funcdefs (append (filter-map (lambda (td) (if (Func? (TopDef-val td)) (FuncDef (TopDef-id td) (TopDef-val td)) #f)) ast)
                            (hash-map LAMBDAS (lambda (k v) (FuncDef (Id k) v))))))
      (hash-clear! LAMBDAS)
      (Program vardefs funcdefs))))

(define LAMBDAS (make-hash))

(define (process-top top-form)
  (kernel-syntax-case top-form #f
    ; We expect a module as the top level form to compile
    [(module id module-path (#%plain-module-begin module-level-form ...))
     (filter-map (lambda (stx) (process-mod stx)) (stx->list #'(module-level-form ...)))]
    [(#%expression expr) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [(begin top-level-form ...) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [(begin-for-syntax top-level-form ...) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [other (process-gtop top-form)]))

; This handles module and submodule form, any false return values mean the form is ignored
(define (process-mod mod-form)
  (kernel-syntax-case mod-form #f
    ; TODO: Build the exports, will need to parse out raw-provide-spec, for now we can just hack all functions to export
    [(#%provide raw-provide-spec ...) #f]
    ; WILLDO?
    [(begin-for-syntax module-level-form ...) (error 'unsupported)]
    [(#%declare declaration-keyword ...) #f]
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
       ; TODO: Support multiple return vals
       (TopDef (first p-ids) p-expr))]
    ; WILLDO? TODO: Ask clements about these
    [(define-syntaxes (id ...) expr) (error 'unsupported)]
    [(#%require raw-require-spec ...) (error 'unsupported)]
    [other (process-expr gtop-form)]))

(define (process-expr expr [named? #f])
  (kernel-syntax-case expr #f
    [(#%plain-lambda formals expr ...)
     (let ((p-formals (process-formal #'formals))
           (p-exprs (filter-map (lambda (stx) (process-expr stx)) (stx->list #'(expr ...)))))
       ; If we are defining a top level lambda then we know it is named
       ;  otherwise the function will be an unnamed lambda
       (if named?
           (Func p-formals p-exprs)
           (let ((id (gensym 'lambda)))
             (hash-set! LAMBDAS id (Func p-formals p-exprs))
             (Id id))))]
    ; TODO: We will just make a lambda for each variation and call it based on the num of args given
    [(case-lambda (formals body) ...)
     (CaseLambda (map (lambda (f b) (Func (process-formal f) (process-expr b)))
                      (stx->list #'(formals ...))
                      (stx->list #'(body ...))))]
    [(if test then else)
     (let ((p-test (process-expr #'test))
           (p-then (process-expr #'then))
           (p-else (process-expr #'else)))
       (If p-test p-then p-else))]
    [(begin exprs ...)
     (let ((p-exprs (filter-map process-expr (stx->list #'(exprs ...)))))
       (Begin p-exprs))]
    [(begin0 expr1 expr2 ...)
     (let ((p-exprs
            (filter-map process-expr (cons #'expr1 (stx->list #'(expr2 ...))))))
       (Begin0 p-exprs))]
    [(let-values ([ids vals] ...) exprs ...)
     (let ((p-ids (filter-map process-expr (stx->list #'(ids ...))))
           (p-vals (filter-map process-expr (stx->list #'(vals ...))))
           (p-exprs (filter-map process-expr (stx->list #'(exprs ...)))))
       (LetVals p-ids p-vals p-exprs))]
    [(letrec-values ([ids vals] ...) exprs ...)
     (let ((p-ids (filter-map process-expr (stx->list #'(ids ...))))
           (p-vals (filter-map process-expr (stx->list #'(vals ...))))
           (p-exprs (filter-map process-expr (stx->list #'(exprs ...)))))
       (LetRecVals p-ids p-vals p-exprs))]
    [(#%plain-app expr ...)
     (let ((p-exprs (filter-map process-expr (stx->list #'(expr ...)))))
       (App (first p-exprs) (rest p-exprs)))]
    [(set! id expr) (Set (process-formal #'id) (process-expr #'expr))]
    [(quote datum) (process-quote #'datum)]
    [id (process-formal #'id)]
    [(#%variable-reference id) (process-formal #'id)]
    [(#%top . id) (TopId (process-formal #'id))]
    [(#%variable-reference (#%top . id)) (TopId (process-formal #'id))]
    ; TODO: WILL NOT DO??
    [(#%variable-reference) '()]
    [(quote-syntax datum) '()]
    [(quote-syntax datum #:local) '()]
    [(with-continuation-mark expr1 expr2 expr3) '()]
    [other (error 'rasm "Unknown expression " expr)]))

(define (process-formal formal)
  (kernel-syntax-case formal #f
    [(id ...) (map (lambda (i) (Id (syntax-e i))) (stx->list #'(id ...)))]
    [id (Id (syntax-e #'id))]
    ; TODO: IS this a fine way to process this? Just treat it all as a list??
    ;  Or should we preserve the pairness?
    [(id1 ... . id2) (append
                      (map (lambda (i) (Id (syntax-e i))) (stx->list #'(id1 ...)))
                      (list (Id (syntax-e #'id2))))]
    [other (error 'rasm "Unknown formal " formal)]))

(define (process-quote datum)
  (if (eof-object? (syntax-e datum))
      '()
      (match (syntax-e datum)
        [(? integer? r) (Int r)]
        [(? real? r) (Float r)]
        [(? boolean? b) (if b (Int 1) (Int 0))]
        [other (error 'unknown (~a datum))])))

