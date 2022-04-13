#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide full-pass)

(struct FEP2 FEP ([init : (Listof Expr)]) #:transparent)
(define-type Env (HashTable Symbol Symbol))
(define env : (HashTable Symbol Id) (make-hash))

(define (full-pass [ast : FEP]) : Module
  (hash-clear! env)
  (typecheck ast)
  (lift-closures (generate-init (uniquify ast))))

; Super simple typecheck but can elaborate later
; Juse make sure no top level definitions overshadow each other
; TODO: What static things must we know that differ betweenm racket and wasm??
(define (typecheck [ast : FEP]) : Void
  (let ((dupes (check-duplicates
                (append
                 (map (lambda ([td : TopDefinition]) (if (Func? td)
                                                         (Id-sym (Func-name td))
                                                         (Id-sym (Var-id td))))
                      (FEP-defs ast))))))
    (when dupes (error 'typecheck (~a "ERROR: Overshadowing Globals: " dupes)))))

; We force globals to be unique, so just go through expressions and uniquify them
(define (uniquify [ast : FEP]) : FEP
  (FEP
     (FEP-provides ast)
     (map (lambda ([td : TopDefinition])
            (if (Func? td)
                (uniquify-func td)
                (Var (Var-id td) (uniquify-expr (Var-expr td)))))
          (FEP-defs ast))))

(define (uniquify-func [func : Func]) : Func
  (begin
    (for-each (lambda ([id : Id]) (hash-set! env (Id-sym id) (Id (gensym (Id-sym id))))) (Func-params func))
    (Func (Func-name func)
          (map (lambda ([id : Id]) (hash-ref env (Id-sym id))) (Func-params func))
          (map (lambda ([expr : Expr]) (uniquify-expr expr)) (Func-body func)))))


(define (uniquify-expr [expr : Expr]) : Expr
  (match expr
    ; TODO: I think we will handle these the same???
    [(LetVals ids vals body) (uniquify-let ids vals body)]
    [(LetRecVals ids vals body)  (uniquify-let ids vals body)]
    [(App fn args) (App (uniquify-id fn) (map uniquify-expr args))]
    [(CaseLambda funcs) (CaseLambda (map uniquify-func funcs))]
    [(If test t f) (If (uniquify-expr test) (uniquify-expr t) (uniquify-expr f))]
    [(Begin exprs) (Begin (map uniquify-expr exprs))]
    [(Begin0 exprs) (Begin0 (map uniquify-expr exprs))]
    [(Set id expr) (Set (uniquify-id id) (uniquify-expr expr))]
    [(Id s) (uniquify-id (Id s))]
    [other expr]))

(define (uniquify-let [ids : (Listof (Listof Id))] [vals : (Listof Expr)] [body : (Listof Expr)]) : LetVals
  (let ((new-ids (map (lambda ([l-id : (Listof Id)])
                           (map (lambda ([id1 : Id] [id2 : Id])
                                  (let ((p-ids (cons (Id-sym id1) (gensym (Id-sym id2)))))
                                    (hash-set! env (car p-ids) (Id (cdr p-ids)))
                                    (Id (cdr p-ids))))
                                l-id
                                l-id))
                         ids)))
       (LetVals new-ids
                (map uniquify-expr vals)
                (map uniquify-expr body))))

(define (uniquify-id [id : Id]) : Id
  (if (hash-has-key? env (Id-sym id)) (hash-ref env (Id-sym id)) id))


(define (generate-init [ast : FEP]) : FEP2
  (FEP2
   (FEP-provides ast)
   (map (lambda ([td : TopDefinition])
          (if (Func? td) td (Var (Var-id td) (Int 0))))
        (FEP-defs ast))
   (filter-map (lambda ([td : TopDefinition])
                 (if (Func? td) #f (Set (Var-id td) (Var-expr td))))
               (FEP-defs ast))))

(define (lift-closures [ast : FEP2]) : Module
  (let ((funcs (filter-map (lambda ([td : TopDefinition]) (if (Func? td) td #f)) (FEP-defs ast)))
        (globals (filter-map (lambda ([td : TopDefinition]) (if (Var? td) (Var-id td) #f)) (FEP-defs ast))))
    (Module (FEP-provides ast)
            (filter-map (lambda ([td : TopDefinition]) (if (Func? td) #f (Var-id td))) (FEP-defs ast))
            (map
             (lambda ([f : Func]) (lift-closure f funcs globals))
             (cons (Func (Id 'init) '() (FEP2-init ast))
                   funcs)))))


; We need to know all the local variables of a function so we NEED TO declare them at the beginning of the function
(define (lift-closure [f : Func] [funcs : (Listof Func)] [globals : (Listof Id)]) : Closure
  (let ((locals (lift-func-locals (Func-body f))))
    (Closure (Func-name f)
             (Func-params f)
             (lift-func-envs (Func-body f)
                             (append
                              (map Func-name funcs)
                              (Func-params f)
                              locals
                              globals))
             locals
             (Func-body f))))

(define (lift-func-locals [body : (Listof Expr)]) : (Listof Id)
  (append-map
   (lambda ([e : Expr]) : (Listof Id)
     (match e
       [(LetVals ids vals body) (lift-let-locals ids vals body)]
       [(LetRecVals ids vals body) (lift-let-locals ids vals body)]
       [(App fn args) (lift-func-locals args)]
       [(If test t f) (append (lift-func-locals (list test))
                              (lift-func-locals (list t))
                              (lift-func-locals (list f)))]
       [(Begin exprs) (lift-func-locals exprs)]
       [(Begin0 exprs) (lift-func-locals exprs)]
       [(Set id expr) (lift-func-locals (list expr))]
       [(Id s) '()]
       [other '()]))
   body))

(define (lift-let-locals [ids : (Listof (Listof Id))] [vals : (Listof Expr)] [body : (Listof Expr)]) : (Listof Id)
  (append
   ; TODO: (first X) hack to handle multiple return vals
   (filter-map
    (lambda ([ids : (Listof Id)] [val : Expr])
      ; TODO: Change this to a type check where we handle functions, we don't want to lift function results
      (if (and (Id? val) (anon? (Id-sym val))) #f (first ids)))
    ids
    vals)
   (lift-func-locals body)))

; We need to lift every env requirement of functions to the list of id's in params
(define (lift-func-envs [body : (Listof Expr)] [known-ids : (Listof Id)]) : (Listof Id)
  (define lfe-helper (lambda ([es : (Listof Expr)]) (lift-func-envs es known-ids)))
  (append-map
   (lambda ([e : Expr]) : (Listof Id)
     (match e
       [(LetVals ids vals body) (append (lfe-helper vals) (lfe-helper body))]
       [(LetRecVals ids vals body) (append (lfe-helper vals) (lfe-helper body))]
       [(App fn args) (lfe-helper args)]
       [(If test t f) (append (lfe-helper (list test)) (lfe-helper (list t)) (lfe-helper (list f)))]
       [(Begin exprs) (lfe-helper exprs)]
       [(Begin0 exprs) (lfe-helper exprs)]
       [(Set id expr) (lfe-helper (list expr))]
       [(Id s)
        (if (findf (lambda ([id : Id]) (equal? (Id-sym id) s)) known-ids) '() (list (Id s)))]
       [other '()]))
   body))


