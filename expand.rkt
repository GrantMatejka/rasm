#lang racket

(require "types.rkt")

(require syntax/kerncase
         syntax/stx
         racket/syntax
         syntax/parse)

(provide expand-file build-ast)

(define TOP_LVL_FORM_ERROR "ERROR: Please provide a module as the top form to compile")

; Opens the specified file and returns fully expanded program
(define (expand-file path)
  (let ((file (open-input-file path)))
    (read-accept-reader #t)
    (read-accept-lang #t)
    
    (define expansion (parameterize ([current-namespace (make-base-namespace)])
                        (expand
                         (read-syntax (object-name file) file))))
    (close-input-port file)
    expansion))

; exp is a fully expanded racket program
(define (build-ast exp)
  (let ((ast (process-top exp)))
    (let-values (((p d) (partition Provide? ast)))
      (FEP p d))))

(define (process-top top-form)
  (kernel-syntax-case top-form #f
    ; We expect a module as the top level form to compile
    [(module id lang (#%plain-module-begin module-level-form ...))
     (flatten (filter-map (lambda (stx) (process-mod stx)) (stx->list #'(module-level-form ...))))]
    [(#%expression expr) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [(begin top-level-form ...) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [(begin-for-syntax top-level-form ...) (error 'process-top-form TOP_LVL_FORM_ERROR)]
    [other (process-gtop top-form)]))

; This handles module and submodule form, any false return values mean the form is ignored
(define (process-mod mod-form)
  (kernel-syntax-case mod-form #f
    [(#%provide spec ...) (append-map parse-provide (syntax->list #'(spec ...)))]
    ; We only expect one sub module max, this happens if the expanded file has #lang
    [(module id module-path (#%plain-module-begin module-level-form ...))
     (if (equal? (syntax-e #'id) 'configure-runtime)
         #f
         (filter-map process-mod (stx->list #'(module-level-form ...))))]
    ; WILLDO?
    [(begin-for-syntax module-level-form ...) (error 'unsupported)]
    [(#%declare declaration-keyword ...) #f]
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
       (if (Func? p-expr)
           (Func (first p-ids) (Func-params p-expr) (Func-body p-expr))
           (Var (first p-ids) p-expr)))]
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
           (Func (Id 'FILL_IN) p-formals p-exprs)
           (Lam p-formals p-exprs)))]
    ; TODO: We will just make a lambda for each variation and call it based on the num of args given
    [(case-lambda (formals body) ...)
     (CaseLambda (map (lambda (f b) (Func (Id 'CL_TODO) (process-formal f) (process-expr b)))
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
    [(id ...) (map get-true-id (stx->list #'(id ...)))]
    [id (get-true-id #'id)]
    ; TODO: IS this a fine way to process this? Just treat it all as a list??
    ;  Or should we preserve the pairness?
    [(id1 ... . id2) (append
                      (map get-true-id (stx->list #'(id1 ...)))
                      (list (get-true-id #'id2)))]
    [other (error 'rasm "Unknown formal " formal)]))

(define (get-true-id id)
  (Id (let ((bound? (identifier-binding id)))
        (match bound?
          [#f (syntax-e id)]
          ['lexical (syntax-e id)]
          [(list (? symbol? s)) s]
          [(list l ...) (second l)]
          [other (error 'unknown "Unknown Symbol ~v" id)]))))

(define (process-quote datum)
  (if (eof-object? (syntax-e datum))
      '()
      (match (syntax-e datum)
        [(? integer? r) (Int r)]
        [(? real? r) (Float r)]
        [(? boolean? b) (if b (Int 1) (Int 0))]
        [(? char? c) (Int (char->integer c))]
        [other (error 'unknown (~a datum))])))


; Borrowed from racketscript @ https://github.com/racketscript/racketscript/blob/master/racketscript-compiler/racketscript/compiler/expand.rkt
(define (parse-provide r)
  (syntax-parse r
    [v:identifier (list (SimpleProvide (syntax-e #'v)))]
    [((~datum for-meta) 0 p ...)
     (stx-map (λ (pv) (SimpleProvide (syntax-e pv))) #'(p ...))]
    [((~datum all-defined)) (list (AllDefined (set)))]
    [((~datum all-defined-except) id ...)
     (list (AllDefined (list->set
                        (stx-map syntax-e #'(id ...)))))]
    [((~datum rename) local-id exported-id)
     (list (RenamedProvide (syntax-e #'local-id)
                           (syntax-e #'exported-id)))]
    [((~datum prefix-all-defined) prefix-id)
     (list (PrefixAllDefined (syntax-e #'prefix-id) (set)))]
    [((~datum prefix-all-defined-except) prefix-id id ...)
     (list (PrefixAllDefined (syntax-e #'prefix-id)
                             (list->set
                              (stx-map syntax-e #'(id ...)))))]
    [((~datum all-from) p ...) '()]
    [((~datum all-from-except) p ...) '()]
    [((~datum for-meta) 1 p ...) '()]
    [((~datum for-syntax) p ...) '()]
    [((~datum protect) p ...)
     (apply append (stx-map parse-provide #'(p ...)))]
    [((~datum struct) p (f ...))
     (append
      (list (SimpleProvide (syntax-e #'p))
            (SimpleProvide (syntax-e (format-id #'p "make-~a" #'p)))
            (SimpleProvide (syntax-e (format-id #'p "struct:~a" #'p)))
            (SimpleProvide (syntax-e (format-id #'p "~a?" #'p))))
      (stx-map ; accessors
       (lambda (f) (syntax-e (format-id #'p "~a-~a" #'p f)))
       #'(f ...))
      (stx-map ; mutators
       (lambda (f) (syntax-e (format-id #'p "set-~a-~a!" #'p f)))
       #'(f ...)))]
    [_ (error "unsupported provide form " (syntax->datum r))]))

; Just like findf but returns user defined value if not found
(define (my-findf lam l [not-found #f])
  (let ((found? (findf lam l)))
    (if found? found? not-found)))

