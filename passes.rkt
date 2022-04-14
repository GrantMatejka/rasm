#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide full-pass)

; Represents a temporary stage of fully expanded programs where we introduce expressions used to initialize global values
(struct FEP2 FEP ([init : (Listof Expr)]) #:transparent)
(define-type Env (HashTable Symbol Id))

(define (full-pass [ast : FEP]) : Module
  (typecheck ast)
  (lift-closures (generate-init (lift-lambdas (uniquify ast)))))

; Super simple typecheck but can elaborate later
; Juse make sure no top level definitions overshadow each other
; TODO: What static things must we know that differ between racket and wasm??
(define (typecheck [ast : FEP]) : Void
  (let ((dupes (check-duplicates
                (append
                 (map (lambda ([td : TopDefinition]) (if (Func? td)
                                                         (Id-sym (Func-name td))
                                                         (Id-sym (Var-id td))))
                      (FEP-defs ast))))))
    (when dupes (error 'typecheck (~a "ERROR: Overshadowing Globals: " dupes)))))

(define (uniquify [ast : FEP]) : FEP
  (FEP
   (FEP-provides ast)
   (map (lambda ([td : TopDefinition])
          (if (Func? td)
              (uniquify-func td)
              ; We force globals to be unique so we don't need to uniquify global ids
              (Var (Var-id td) (uniquify-expr (Var-expr td) (make-hash '())))))
        (FEP-defs ast))))

(define (uniquify-func [func : Func]) : Func
  (let ((starting-env (make-hash (map (lambda ([id : Id]) (cons (Id-sym id) (Id (gensym (Id-sym id))))) (Func-params func)))))
    (Func (Func-name func)
          (map (lambda ([id : Id]) (hash-ref starting-env (Id-sym id))) (Func-params func))
          (map (lambda ([expr : Expr]) (uniquify-expr expr starting-env)) (Func-body func)))))


(define (uniquify-expr [expr : Expr] [env : Env]) : Expr
  (define ue-helper (lambda ([e : Expr]) (uniquify-expr e env)))
  (match expr
    [(Lam params body) (Lam (map (lambda ([i : Id]) (uniquify-id i env)) params) (map ue-helper body))]
    ; TODO: I think we will handle these the same???
    ;TODO: LetRec needs a new-env for each val expression
    [(LetVals ids vals body) (uniquify-let ids vals body env)]
    [(LetRecVals ids vals body) (uniquify-letrec ids vals body env)]
    ; The env will hold the name of any lambdas we lift
    [(App fn args) (App (uniquify-l/id fn env) (map ue-helper args))]
    [(CaseLambda funcs) (CaseLambda (map uniquify-func funcs))]
    [(If test t f) (If (ue-helper test) (ue-helper t) (ue-helper f))]
    [(Begin exprs) (Begin (map ue-helper exprs))]
    [(Begin0 exprs) (Begin0 (map ue-helper exprs))]
    [(Set id expr) (Set (uniquify-id id env) (ue-helper expr))]
    [(Id s) (uniquify-id (Id s) env)]
    [other expr]))

(define (uniquify-l/id [l/i : (U Lam Id)] [env : Env]) : (U Lam Id)
  (match l/i
    [(Lam params body) (Lam (map (lambda ([i : Id]) (uniquify-id i env)) params)
                            (map (lambda ([e : Expr]) (uniquify-expr e env)) body))]
    [(Id s) (uniquify-id (Id s) env)]
    [other (error 'uniquify "Unknown ~a" l/i)]))

(define (uniquify-let [ids : (Listof (Listof Id))] [vals : (Listof Expr)] [body : (Listof Expr)] [env : Env]) : LetVals
  (let ((new-env (hash-copy env)))
    (let ((new-ids (map (lambda ([l-id : (Listof Id)])
                          (map (lambda ([id1 : Id] [id2 : Id])
                                 (let ((p-ids (cons (Id-sym id1) (gensym (Id-sym id2)))))
                                   (hash-set! new-env (car p-ids) (Id (cdr p-ids)))
                                   (Id (cdr p-ids))))
                               l-id
                               l-id))
                        ids)))
      (LetVals new-ids
               (map (lambda ([e : Expr]) (uniquify-expr e env)) vals)
               (map (lambda ([e : Expr]) (uniquify-expr e new-env)) body)))))

; Just like uniquify-let but we pass the new-env to each of the expr's
(define (uniquify-letrec [ids : (Listof (Listof Id))] [vals : (Listof Expr)] [body : (Listof Expr)] [env : Env]) : LetVals
  (let ((new-env (hash-copy env)))
    (let ((new-ids (map (lambda ([l-id : (Listof Id)])
                          (map (lambda ([id1 : Id] [id2 : Id])
                                 (let ((p-ids (cons (Id-sym id1) (gensym (Id-sym id2)))))
                                   (hash-set! new-env (car p-ids) (Id (cdr p-ids)))
                                   (Id (cdr p-ids))))
                               l-id
                               l-id))
                        ids)))
      (LetVals new-ids
               (map (lambda ([e : Expr]) (uniquify-expr e new-env)) vals)
               (map (lambda ([e : Expr]) (uniquify-expr e new-env)) body)))))

(define (uniquify-id [id : Id] [env : Env]) : Id
  (if (hash-has-key? env (Id-sym id)) (hash-ref env (Id-sym id)) id))

(define LAMBDAS : (Listof Func) '())
(define (lift-lambdas [ast : FEP]) : FEP
  (set! LAMBDAS '())
  (let ((var-lams (map lift-var-lambdas
                       (filter-map (lambda ([td : TopDefinition]) (if (Var? td) td #f)) (FEP-defs ast)))))
    (FEP
     (FEP-provides ast)
     (append
      (map (lambda ([td : TopDefinition])
             (if (Func? td) (lift-func-lambdas td) (lift-var-lambdas td)))
           (FEP-defs ast))
      LAMBDAS))))

(define (lift-func-lambdas [f : Func]) : Func
  (Func (Func-name f)
        (Func-params f)
        (map lift-expr-lambdas (Func-body f))))

(define (lift-expr-lambdas [e : Expr]) : Expr
  (match e
    [(Lam params body)
     (let ((name (Id (gensym '__lambda))))
       (set! LAMBDAS (cons (Func name params body) LAMBDAS))
       name)]
    [(LetVals ids vals body) (LetVals ids
                                      (map lift-expr-lambdas vals)
                                      (map lift-expr-lambdas body))]
    [(LetRecVals ids vals body) (LetRecVals ids
                                            (map lift-expr-lambdas vals)
                                            (map lift-expr-lambdas body))]
    [(App fn args) (App (lift-l/i-lambdas fn)
                        (map lift-expr-lambdas args))]
    [(CaseLambda funcs) (CaseLambda (map lift-func-lambdas funcs))]
    [(If test t f) (If (lift-expr-lambdas test) (lift-expr-lambdas t) (lift-expr-lambdas f))]
    [(Begin exprs) (Begin (map lift-expr-lambdas exprs))]
    [(Begin0 exprs) (Begin0 (map lift-expr-lambdas exprs))]
    [(Set id expr) (Set id (lift-expr-lambdas expr))]
    [(Id s) (Id s)]
    [other e]))

(define (lift-l/i-lambdas [l/i : (U Lam Id)]) : Id
  (match l/i
    [(Lam params body)
     (let ((name (Id (gensym '__lambda))))
       (set! LAMBDAS (cons (Func name params body) LAMBDAS))
       name)]
    [(Id s) (Id s)]
    [other (error 'lift-lambdas "Unknown ~a" l/i)]))

(define (lift-var-lambdas [v : Var]) : Var
  (Var (Var-id v) (lift-expr-lambdas (Var-expr v))))

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


