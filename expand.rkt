#lang racket/base

;; Ripped out of Pycket project. Orignial source code at -
;;
;;   https://github.com/samth/pycket/blob/master/pycket/pycket-lang/expand.rkt
;;
;; Copyright (c) 2013 Sam Tobin-Hochstadt, Jeremy Siek, Carl Friedrich Bolz

(require (for-syntax racket/base)
         racket/bool
         racket/dict
         (only-in racket/list
                  append-map
                  last-pair
                  filter-map
                  first
                  add-between)
         racket/list
         racket/match
         racket/path
         racket/pretty
         racket/set
         racket/syntax
         racket/vector
         syntax/id-table
         syntax/parse
         syntax/stx
         version/utils
         "absyn.rkt"
         "config.rkt"
         "global.rkt"
         "logging.rkt"
         "moddeps.rkt"
         "util.rkt")

(provide convert
         open-read-module
         read-module
         to-absyn
         to-absyn/top
         read-and-expand-module
         quick-expand)

(define current-module (make-parameter #f))
(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))
(define lexical-bindings (make-parameter #f)) ;; Free-Id-Table
(define defined-names (make-parameter #f)) ;; set of symbols, rkt prog can have defs with same name
(define dup-names (make-parameter #f)) ;; free-id-table, rkt prog can have defs with same name
(define skip-freshening? (make-parameter #f))

;;;----------------------------------------------------------------------------
;;;; Module dependencies and imports

;; (Setof (U ModulePath Symbol))
(define current-module-imports (make-parameter (set)))

;;;----------------------------------------------------------------------------
;;;; Module paths

(define (index->path i)
  (define-values (v u) (module-path-index-split i))
  (if v
      (list (resolved-module-path-name (module-path-index-resolve i)) #f)
      (list (current-module) #t)))

;;;-----------------------------------------------------------------------------
;;;; Conversion and expansion

;; record all (symbolic) names defined via define-values
;; - need to track these because Racket progs can have multiple defs with same name
(define (register-defined-names! stx)
  (cond
    [(stx-list? stx)
     (for-each register-defined-names! (syntax->list stx))]
    [(identifier? stx)
     (if (set-member? (defined-names) (syntax-e stx))
         (register-dup-name! stx) ; add to dup-names set if already seen
         (set-add! (defined-names) (syntax-e stx)))] ; else add to defined-names
    [(stx-pair? stx)
     (register-defined-names! (stx-car stx))
     (register-defined-names! (stx-cdr stx))]
    [else (error 'register-defined-names "unexpected ~a" stx)]))

;; registers duplicate names that need to be freshened in the next pass
(define (register-dup-name! id)
  (dict-set! (dup-names) id (generate-temporary id)))

(define (register-lexical-bindings! stx)
  (cond
    [(stx-list? stx)
     (for-each register-lexical-bindings! (syntax->list stx))]
    [(identifier? stx)
     (dict-set! (lexical-bindings)
                stx
                (if (skip-freshening?)
                    stx
                    (car (generate-temporaries (list stx)))))]
    [(stx-pair? stx)
     (register-lexical-bindings! (stx-car stx))
     (register-lexical-bindings! (stx-cdr stx))]
    [else (error 'register-lexical-bindings "unexpected ~a" stx)]))

(define (get-freshened-lexical-binding! id)
  (dict-ref! (lexical-bindings)
             id
             (λ _ (error 'get-freshened-lexical-binding! "Missed binding: ~a" id))))

#;(define (require-parse r)
  (syntax-parse r
    [v:str (Require (syntax-e #'v) #f)]
    [v:identifier (Require (syntax-e #'v) #f)]
    [_ (error "unsupported require format")]))

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
    [_ #;(error "unsupported provide form " (syntax->datum r)) '()]))

(define (formals->absyn formals)
  (cond
    [(stx-list? formals) (stx-map formals->absyn formals)]
    [(stx-pair? formals)
     ;; Splits the formals to be compatible with
     ;; LetValues/PlainLambda.  If we reach here, rest of the
     ;; structure will also reach here unless its terminal. We build
     ;; up proper part and improper part result recursively.
     (cond
       [(stx-pair? (stx-cdr formals))
        (match-define (cons pos rst) (formals->absyn (stx-cdr formals)))
        (cons (cons (formals->absyn (stx-car formals)) pos)
              rst)]
       [else (cons (list (formals->absyn (stx-car formals)))
                   (formals->absyn (stx-cdr formals)))])]
    [(identifier? formals)
     (syntax-e (get-freshened-lexical-binding! formals))]
    [(null? formals) null]
    [else 'formals->absyn "Invalid formals: ~a" formals]))

(define (to-absyn v)
  (syntax-parse v
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [v:str (syntax-e #'v)]
    ;; special case when under quote to avoid the "interesting"
    ;; behavior of various forms
    [(_ ...)
     #:when (quoted?)
     (map to-absyn (syntax->list v))]
    [(module _ ...) #f] ;; ignore these
    [(module* _ ...) #f] ;; ignore these
    ;; this is a simplification of the json output
    [(#%plain-app e0 e ...)
     (PlainApp (to-absyn #'e0) (map to-absyn (syntax->list #'(e ...))))]
    [(#%expression e) (to-absyn #'e)]
    [(begin e ...)
     (map to-absyn (syntax->list #'(e ...)))]
    [(begin0 e0 e ...)
     (Begin0
       (to-absyn #'e0)
       (map to-absyn (syntax->list #'(e ...))))]
    [(if e0 e1 e2)
     (If (to-absyn #'e0) (to-absyn #'e1) (to-absyn #'e2))]
    [(let-values ([xs es] ...) b ...)
     (register-lexical-bindings! #'(xs ...))
     (LetValues (for/list ([x (syntax-e #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (formals->absyn x)
                        (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(letrec-values ([xs es] ...) b ...)
     (register-lexical-bindings! #'(xs ...))
     (LetValues (for/list ([x (syntax->list #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (formals->absyn x)
                        (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(quote e) (Quote
                (parameterize ([quoted? #t])
                  (to-absyn #'e)))] ;;;; TODO: HACK! See what actually happens
    [(quote-syntax e ...) '()]
    [(#%require x ...)
     #f
     #;(map require-parse (syntax->list #'(x ...)))]
    [(#%provide x ...)
     (append-map parse-provide (syntax->list #'(x ...)))]
    [(case-lambda . clauses)
     (CaseLambda
      (stx-map (λ (c)
                 (syntax-parse c
                   [(formals . body)
                    (register-lexical-bindings! #'formals)
                    (PlainLambda (formals->absyn #'formals)
                                 (stx-map to-absyn #'body)
                                 #f)]))
               #'clauses))]
    [(#%plain-lambda formals . body)
     (register-lexical-bindings! #'formals)
     (define unchecked? (syntax-property v 'racketscript-unchecked-lambda?))
     (define fabsyn (formals->absyn #'formals))
     (PlainLambda fabsyn (map to-absyn (syntax->list #'body)) unchecked?)]
    [(define-values (name)
       (#%plain-app (~datum #%js-ffi) (quote (~datum require)) (quote mod:str)))
     ;; HACK: Special case for JSRequire
     (JSRequire (syntax-e #'name) (syntax-e #'mod) 'default)]
    [(define-values (name)
       (#%plain-app (~datum #%js-ffi) (quote (~datum require)) (quote *) (quote mod:str)))
     ;; HACK: Special case for JSRequire
     (JSRequire (syntax-e #'name) (syntax-e #'mod) '*)]
    [(define-values (id ...) b)
     (DefineValues (syntax->datum #'(id ...)) (to-absyn #'b))]
    [(#%top . x) (TopId (syntax-e #'x))]
    [(#%variable-reference x) (VarRef (to-absyn #'x))]
    [(#%variable-reference) (VarRef #f)]
    [i:identifier #:when (quoted?) (syntax-e #'i)]
    [i:identifier
     (define (rename-module mpath)
       ;; Rename few modules for simpler compilation
       (cond
         [(symbol? mpath) (list #f mpath)]
         #;[(collects-module? mpath) (list #t '#%kernel)]
         [else (list #f mpath)]))

     (match (identifier-binding #'i)
       ['lexical (LocalIdent (syntax-e (get-freshened-lexical-binding! #'i)))]
       [#f (TopLevelIdent (syntax-e #'i))]
       [(list src-mod src-id nom-src-mod mod-src-id src-phase import-phase nominal-export-phase)
        ;; from where we import
        (match-define (list src-mod-path-orig self?) (index->path src-mod))
        (match-define (list nom-src-mod-path-orig _) (index->path nom-src-mod))
        (match-define (list module-renamed? src-mod-path) (rename-module src-mod-path-orig))

        (cond
          [self? (LocalIdent (syntax-e #'i))]
          [else
           ;; Add the module from where we actual import this, so that we import this, and
           ;; any side-effects due to this module is actually executed
           ;(match-define (list nom-mod-path _) (index->path nom-src-mod))
           ;(current-module-imports (set-add (current-module-imports) nom-mod-path))

           ;; And still add the actual module where identifier is defined for easy
           ;; and compact import. NOTE:In future we may want to remove this and
           ;; compute this with moddeps information.
           ;; FIXME?: Racket7 workaround
           (if (and (version<=? "7.0" (version))
                    (equal? src-mod-path '#%runtime))
               (current-module-imports (set-add (current-module-imports) '#%kernel))
               (current-module-imports (set-add (current-module-imports) src-mod-path)))

           ;;HACK: See test/struct/import-struct.rkt. Somehow, the
           ;;  struct constructor has different src-id returned by
           ;;  identifier-binding than the actual identifier name used
           ;;  at definition site. Implicit renaming due to macro
           ;;  expansion?
           ;;
           ;;HACK: See tests/modules/rename-and-import.rkt and
           ;;  tests/rename-from-primitive.rkt. When importing from a
           ;;  module with a rename, identifier-binding's, mod-src-id
           ;;  shows the renamed id, i.e. the one it is imported as
           ;;  instead of what it is exported as. We handle this
           ;;  special case where both src-mod and nom-src-mod are
           ;;  equal. If both source module and normalized module are
           ;;  same with different ids:
           ;;  - Check if nom-src-id is exported and use that. Or,
           ;;  - Check if src-id is exported and use that. Or,
           ;;  - Fallback to module source id.
           (define-values (id-to-follow path-to-symbol)
             (cond
               [(and (equal? src-mod nom-src-mod)
                     (not (equal? src-id mod-src-id)))
                (let ([path-to-symbol-src (follow-symbol (global-export-graph)
                                                         nom-src-mod-path-orig
                                                         mod-src-id)])
                  (if path-to-symbol-src
                      (values mod-src-id path-to-symbol-src)
                      (let ([path-to-symbol-nom (follow-symbol (global-export-graph)
                                                               nom-src-mod-path-orig
                                                               src-id)])
                        (if path-to-symbol-nom
                            (values src-id path-to-symbol-nom)
                            (values mod-src-id #f)))))]
               [else (values mod-src-id
                             (follow-symbol (global-export-graph)
                                            nom-src-mod-path-orig
                                            mod-src-id))]))

           ;; If the module is renamed use the id name used at the importing
           ;; module rather than defining module. Since renamed, module currently
           ;; are #%kernel which we write ourselves in JS we prefer original name.
           ;; TODO: We potentially might have clashes, but its unlikely.
           (define-values (effective-id effective-mod reachable?)
             (cond
               [module-renamed? (values mod-src-id src-mod-path #t)]
               [(false? path-to-symbol)
                (when (and (not (ignored-undefined-identifier? #'i))
                           (symbol? src-mod-path))
                  ;; Since free id's are anyway caught by Racket, just
                  ;; complain about the primitives.
                  (log-rjs-warning
                   "Implementation of identifier ~a not found in module ~a!"
                   (syntax-e #'i) src-mod-path))
                (values id-to-follow src-mod-path #f)]
               [else
                (match-let ([(cons (app last mod) (? symbol? id)) path-to-symbol])
                  (values id mod #t))]))

           (ImportedIdent effective-id effective-mod reachable?)])])]
    [(define-syntaxes (i ...) b) #f]
    [(set! s e)
     (let ([id* (if (equal? (identifier-binding #'s) 'lexical)
                    (get-freshened-lexical-binding! #'s)
                    #'s)])
       (Set! (syntax-e id*) (to-absyn #'e)))]
    [(with-continuation-mark key value result)
     (WithContinuationMark (to-absyn #'key)
                           (to-absyn #'value)
                           (to-absyn #'result))]
    [(begin-for-syntax b ...) #f]
    [(_ ...)
     (map to-absyn (syntax->list v))]
    [(a . b)
     (cons (to-absyn #'a) (to-absyn #'b))]
    [#(_ ...) (vector-map to-absyn (syntax-e v))]
    [_ #:when (hash? (syntax-e v))
       (define val (syntax-e v))
       (define keys (to-absyn (datum->syntax #'lex (hash-keys val))))
       (define vals (to-absyn (datum->syntax #'lex (hash-values val))))
       (define hash-maker
         (cond
           [(hash-eq? val) make-immutable-hasheq]
           [(hash-eqv? val) make-immutable-hasheqv]
           [(hash-equal? val) make-immutable-hash]))
       (hash-maker (map cons keys vals))]
    [_ #:when (number? (syntax-e v)) (syntax-e v)]
    [_ #:when (bytes? (syntax-e v)) (syntax-e v)]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (prefab-struct-key (syntax-e v)) #f] ;; TODO: No error to compile FFI
    [_ #:when (box? (syntax-e v)) (box (parameterize ([quoted? #t])
                                         (to-absyn (unbox (syntax-e v)))))]
    [_ #:when (exact-integer? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (boolean? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (keyword? (syntax-e v)) (Quote (syntax-e v))]
    [(~or (~datum +inf.0) (~datum -inf.0) (~datum nan.0))
     (Quote (syntax-e v))]
    [_ #:when (real? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (complex? (syntax-e v)) #f]
    [_ #:when (char? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (regexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (pregexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (byte-regexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (byte-pregexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (void? (syntax-e v))
       (Quote (void))]
    [_ (displayln "unsupported form =>")
       (pretty-print (syntax->datum v))
       (error 'expand)]))

(define (freshen-form v)
  (syntax-parse v
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(define-values (name) (#%plain-app (~datum #%js-ffi) . _))
     ;; HACK: Special case for JSRequire
     this-syntax]
    [(define-values (id ...) . _)
     (register-defined-names! #'(id ...))
     this-syntax]
    [_ this-syntax]))

(define (convert mod path)
  (syntax-parse (freshen-mod-forms mod) ; Racket can have 2+ defs with same name
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([current-module path]
                    [current-module-imports (set)]
                    [current-directory (path-only path)]
                    [lexical-bindings (make-free-id-table)])
       (define mod-id (syntax-e #'name))
       (log-rjs-info "[absyn] ~a" mod-id)
       (let* ([ast (filter-map to-absyn (syntax->list #'(forms ...)))]
              [imports (current-module-imports)]
              [quoted-bindings (list->set
                                (map
                                 syntax-e
                                 (filter
                                  (λ (x)
                                    ;; We just compile phase 0 forms now
                                    (let ([r (identifier-binding x)])
                                      (and (list? r)
                                           (zero? (list-ref (identifier-binding x) 4)))))
                                  (get-quoted-bindings #'(forms ...)))))])
         (Module mod-id
                 path
                 (syntax->datum #'lang)
                 imports
                 quoted-bindings
                 ast)))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

(define (freshen-mod-forms mod)
  (syntax-parse mod
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([defined-names (mutable-set)]
                    [dup-names (make-free-id-table)])
       (define mod-id (syntax-e #'name))
       (log-rjs-info "[freshening module forms] ~a" mod-id)
       (for-each freshen-form (syntax->list #'(forms ...)))
       (replace-dup-names this-syntax))]
    [_ (error 'freshen-mod-forms "bad ~a ~a" mod (syntax->datum mod))]))

;; replace all ids in (dup-names) with fresh name;
;; handles modules that have multiple defs with the same symbolic name
(define (replace-dup-names stx)
  (cond
    [(pair? stx) (cons (replace-dup-names (car stx))
                       (replace-dup-names (cdr stx)))]
    [(stx-pair? stx)
     (datum->syntax stx (cons (replace-dup-names (stx-car stx))
                              (replace-dup-names (stx-cdr stx)))
                    stx stx stx)]
    [(and (identifier? stx) (dict-has-key? (dup-names) stx))
     (dict-ref (dup-names) stx)]
    [else stx]))

(define (to-absyn/top stx)
  (to-absyn stx))

(define (do-expand stx)
  ;; error checking
  (syntax-parse stx
    [((~and mod-datum (~datum module)) n:id lang:expr . rest)
     (void)]
    [((~and mod-datum (~datum module)) . rest)
     (error 'do-expand
            "got ill-formed module: ~a\n" (syntax->datum #'rest))]
    [rest
     (error 'do-expand
            "got something that isn't a module: ~a\n" (syntax->datum #'rest))])
  ;; work

  (parameterize ([current-namespace (make-base-namespace)])
    (expand stx)))

;;; Read modules

(define (read-module input)
  (read-syntax (object-name input) input))

(define (open-read-module in-path)
  (call-with-input-file (actual-module-path in-path)
    (λ (in)
      (read-module in))))

(define (quick-expand in-path)
  (log-rjs-info "[expand] ~a" in-path)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (define full-path (path->complete-path (actual-module-path in-path)))
  (parameterize ([current-directory (path-only full-path)])
    (do-expand (open-read-module in-path))))

(define (read-and-expand-module input)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (do-expand (read-syntax (object-name input) input)))

;;;----------------------------------------------------------------------------
;;; Flatten Phases in Module

(define (flatten-module-forms mod-stx)
  (define phase-forms (make-hash))

  (define (save-form! form)
    (hash-update! phase-forms
                  (current-phase)
                  (λ (v)
                    (append v (list form)))
                  '()))

  (define (walk stx)
    (syntax-parse stx
      #:literal-sets ((kernel-literals #:phase (current-phase)))
      [((begin-for-syntax forms ...) . tl)
       (parameterize ([current-phase (add1 (current-phase))])
         (walk #'(forms ...)))
       (walk #'tl)]
      [(hd . tl)
       (save-form! #'hd)
       (walk #'tl)]
      [() (void)]
      [_ (save-form! stx)]))

  (parameterize ([current-phase 0])
    (walk mod-stx))

  (for/hash ([(phase forms) phase-forms])
    (values phase (datum->syntax mod-stx forms))))

;;;----------------------------------------------------------------------------
;;; Prepare binding dependency graph

(define (get-quoted-bindings stx)
  (syntax-parse stx
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(quote-syntax e)
     (parameterize ([quoted? #t]
                    [current-phase (sub1 (current-phase))])
       (get-quoted-bindings #'e))]
    [(define-syntaxes _ b)
     (parameterize ([current-phase (add1 (current-phase))])
       (get-quoted-bindings #'b))]
    [(begin-for-syntax forms ...)
     (parameterize ([current-phase (add1 (current-phase))])
       (get-quoted-bindings #'(forms ...)))]
    [(hd . tl)
     (append (get-quoted-bindings #'hd)
             (get-quoted-bindings #'tl))]
    [() '()]
    [v:id #:when (quoted?)
          (match (identifier-binding #'v (current-phase))
            [(list src-mod src-id _ _ src-phase _ _)
             (define-values (v u) (module-path-index-split src-mod))
             (cond
               [(and (false? v) (false? u))
                (unless (equal? src-phase (current-phase))
                  (error 'get-quoted-binding
                         "Identifier phase is ~a, expecte ~a."
                         src-phase (current-phase)))
                (list stx)]
               [v '()])]
            [_ '()])]
    [_ '()]))

;;;----------------------------------------------------------------------------



