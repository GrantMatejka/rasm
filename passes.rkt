#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide full-pass)

(define (full-pass [ast : FEP]) : Module
  (typecheck ast)
  (discover-types (lift-closures (generate-init (lift-lambdas (generate-unique-ids ast))))))

; Juse make sure no top level definitions overshadow each other
(define (typecheck [ast : FEP]) : Void
  (let ((dupes (check-duplicates
                (append
                 (map (lambda ([td : TopDefinition]) (if (Func? td) (Func-name td) (Var-id td)))
                      (FEP-defs ast))))))
    (when dupes (error 'typecheck (~a "ERROR: Overshadowing Globals: " dupes)))))

(define (generate-unique-ids [ast : FEP]) : FEP
  (FEP
   (FEP-provides ast)
   (map (lambda ([td : TopDefinition])
          (if (Func? td)
              (uniquify-func td)
              ; We force globals to be unique so we don't need to uniquify global ids
              (Var (Var-id td) (uniquify-expr (Var-expr td) (make-hash '())))))
        (FEP-defs ast))))

(define (uniquify-func [func : Func]) : Func
  (let ((starting-env (make-hash (map (lambda ([id : Symbol]) (cons id (gensym id))) (Func-params func)))))
    (Func (Func-name func)
          (map (lambda ([id : Symbol]) (hash-ref starting-env id)) (Func-params func))
          (Func-env func)
          (map (lambda ([expr : L0-Expr]) (uniquify-expr expr starting-env)) (Func-body func)))))

(define (uniquify-expr [expr : L0-Expr] [env : Env]) : L0-Expr
  (define ue-helper (lambda ([e : L0-Expr]) (uniquify-expr e env)))
  (match expr
    [(L0-Lam params body) (let ((new-env (hash-copy env)))
                            (let ((new-params (map (lambda ([i : Symbol])
                                                     (let ((p-ids (cons i (gensym i))))
                                                       (hash-set! new-env (car p-ids) (cdr p-ids))
                                                       (cdr p-ids)))
                                                   params)))
                              (L0-Lam new-params (map (lambda ([e : L0-Expr]) (uniquify-expr e new-env)) body))))]
    [(L0-LetVals ids vals body) (uniquify-let ids vals body env)]
    [(L0-LetRecVals ids vals body) (uniquify-letrec ids vals body env)]
    ; The env will hold the name of any lambdas we lift
    [(L0-App fn args) (L0-App (ue-helper fn) (map ue-helper args))]
    [(L0-CaseLambda funcs) (L0-CaseLambda (map ue-helper funcs))]
    [(L0-If test t f) (L0-If (ue-helper test) (ue-helper t) (ue-helper f))]
    [(L0-Begin exprs) (L0-Begin (map ue-helper exprs))]
    [(L0-Begin0 exprs) (L0-Begin0 (map ue-helper exprs))]
    [(L0-Set id expr) (L0-Set (uniquify-id id env) (ue-helper expr))]
    [(? symbol? s) (uniquify-id s env)]
    [other expr]))

(define (uniquify-let [ids : (Listof (Listof Symbol))]
                      [vals : (Listof L0-Expr)]
                      [body : (Listof L0-Expr)]
                      [env : Env]) : L0-LetVals
  (let ((new-env (hash-copy env)))
    (let ((new-ids (map (lambda ([l-id : (Listof Symbol)])
                          (map (lambda ([id1 : Symbol] [id2 : Symbol])
                                 (let ((p-ids (cons id1 (gensym id2))))
                                   (hash-set! new-env (car p-ids) (cdr p-ids))
                                   (cdr p-ids)))
                               l-id
                               l-id))
                        ids)))
      (L0-LetVals new-ids
                  (map (lambda ([e : L0-Expr]) (uniquify-expr e env)) vals)
                  (map (lambda ([e : L0-Expr]) (uniquify-expr e new-env)) body)))))

; Just like uniquify-let but we pass the new-env to each of the expr's
(define (uniquify-letrec [ids : (Listof (Listof Symbol))]
                         [vals : (Listof L0-Expr)]
                         [body : (Listof L0-Expr)]
                         [env : Env]) : L0-LetVals
  (let ((new-env (hash-copy env)))
    (let ((new-ids (map (lambda ([l-id : (Listof Symbol)])
                          (map (lambda ([id1 : Symbol] [id2 : Symbol])
                                 (let ((p-ids (cons id1 (gensym id2))))
                                   (hash-set! new-env (car p-ids) (cdr p-ids))
                                   (cdr p-ids)))
                               l-id
                               l-id))
                        ids)))
      (L0-LetVals new-ids
                  (map (lambda ([e : L0-Expr]) (uniquify-expr e new-env)) vals)
                  (map (lambda ([e : L0-Expr]) (uniquify-expr e new-env)) body)))))

(define (uniquify-id [id : Symbol] [env : Env]) : Symbol
  (if (hash-has-key? env id) (hash-ref env id) id))

; TODO: Get Lambda env's working better

; In order to lift lambdas, we need to identify every lambda form and capture the current environment at lift time
(define LAMBDAS : (Listof Func) '())
(define (lift-lambdas [ast : FEP]) : FEP
  (set! LAMBDAS '())
  ; Top id's are any globals/top level function names
  (let ((TOP_IDS (append
                  (hash-keys prims)
                  (map
                   (lambda ([td : TopDefinition])
                     (if (Func? td) (Func-name td) (Var-id td)))
                   (FEP-defs ast)))))
    (FEP
     (FEP-provides ast)
     (append
      (map (lambda ([td : TopDefinition])
             (if (Func? td) (lift-func-lambdas td TOP_IDS '()) (lift-var-lambdas td TOP_IDS)))
           (FEP-defs ast))
      LAMBDAS))))


(define (lift-func-lambdas [f : Func] [top-ids : (Listof Symbol)] [env : (Listof Symbol)]) : Func
  (Func (Func-name f)
        (Func-params f)
        env
        (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids (Func-params f))) (Func-body f))))

(define (lift-var-lambdas [v : Var] [top-ids : (Listof Symbol)]) : Var
  (Var (Var-id v) (lift-expr-lambdas (Var-expr v) top-ids '())))

; Lifting lambdas will result in our final IR
(define (lift-expr-lambdas [e : L0-Expr] [top-ids : (Listof Symbol)] [env : (Listof Symbol)]) : L0-Expr
  (match e
    ; If we get a lambda, we can just directly lift it as a WebAssembly function
    [(L0-Lam params body)
     (let ((name (gensym '__lambda))
           (new-env (append params env)))
       (set! LAMBDAS (cons (Func name params env (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) body)) LAMBDAS))
       name)]
    [(L0-CaseLambda funcs)
     (let ((name (gensym '__lambda))
           (new-env env))
       (set! LAMBDAS (cons (Func name '() env
                                 (list (L0-CaseLambda
                                        (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) funcs))))
                           LAMBDAS))
       name)]
    ; TODO: Add ids to known-ids and remove them from unknown ids
    [(L0-LetVals ids vals body)
     (let ((new-env (append env (cast (flatten ids) (Listof Symbol)))))
       (L0-LetVals ids
                   (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) vals)
                   (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) body)))]
    ; TODO: Add ids to known-ids and remove them from unknown ids
    [(L0-LetRecVals ids vals body)
     (let ((new-env (append env (cast (flatten ids) (Listof Symbol)))))
       (L0-LetRecVals ids
                      (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) vals)
                      (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids new-env)) body)))]
    ; If we are ever applying an expression, make it a closure
    [(L0-App expr args) (L0-App (lift-expr-lambdas expr top-ids env) (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids env)) args))]
    [(L0-If test t f) (L0-If (lift-expr-lambdas test top-ids env) (lift-expr-lambdas t top-ids env) (lift-expr-lambdas f top-ids env))]
    [(L0-Begin exprs) (L0-Begin (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids env)) exprs))]
    [(L0-Begin0 exprs) (L0-Begin0 (map (lambda ([e : L0-Expr]) (lift-expr-lambdas e top-ids env)) exprs))]
    [(L0-Set id expr) (L0-Set id (lift-expr-lambdas expr top-ids env))]
    [(? symbol? s) s]
    [other e]))

(define (generate-init [ast : FEP]) : FEP2
  (FEP2
   (FEP-provides ast)
   (map (lambda ([td : TopDefinition])
          (if (Func? td) td (Var (Var-id td) (Int 0))))
        (FEP-defs ast))
   (filter-map (lambda ([td : TopDefinition])
                 (if (Func? td) #f (L0-Set (Var-id td) (Var-expr td))))
               (FEP-defs ast))))

(define (lift-closures [ast : FEP2]) : Module
  (let ((funcs (filter-map (lambda ([td : TopDefinition]) (if (Func? td) td #f)) (FEP-defs ast)))
        (globals (filter-map (lambda ([td : TopDefinition]) (if (Var? td) (Var-id td) #f)) (FEP-defs ast))))
    (Module (FEP-provides ast)
            (filter-map (lambda ([td : TopDefinition]) (if (Func? td) #f (Id (Var-id td) 'UNDEFINED))) (FEP-defs ast))
            (map
             (lambda ([f : Func]) (lift-closure f funcs globals))
             (cons (Func 'init '() '() (FEP2-init ast))
                   funcs)))))

; We need to know all the local variables of a function so we NEED TO declare them at the beginning of the function
(define (lift-closure [f : Func] [funcs : (Listof Func)] [globals : (Listof Symbol)]) : Closure
  (let ((body (map L0-expr->Expr (Func-body f))))
    (let ((locals (lift-func-locals body)))
      (Closure (Func-name f)
               (map sym->temp-id (Func-params f))
               (map sym->temp-id (Func-env f))
               (map sym->temp-id locals)
               body))))

(define (L0-expr->Expr [e : L0-Expr]) : Expr
  (match e
    [(L0-LetVals ids vals body) (LetVals ids
                                         (map L0-expr->Expr vals)
                                         (map L0-expr->Expr body))]
    [(L0-LetRecVals ids vals body) (LetRecVals ids
                                               (map L0-expr->Expr vals)
                                               (map L0-expr->Expr body))]
    [(L0-App fn args)
     (let ((p-args (map L0-expr->Expr args)))
       (match fn
         [(? symbol? fn) (IndirectCall fn p-args)]
         [other (Call '__app (append (list (L0-expr->Expr fn)) p-args))]))]
    [(L0-CaseLambda funcs) (CaseLambda (filter-map (lambda ([e : L0-Expr]) (if (symbol? e) e #f)) funcs))]
    [(L0-If test t f) (If (L0-expr->Expr test) (L0-expr->Expr t) (L0-expr->Expr f))]
    [(L0-Begin exprs) (Begin (map L0-expr->Expr exprs))]
    [(L0-Begin0 exprs) (Begin0 (map L0-expr->Expr exprs))]
    [(L0-Set id expr) (Set id (L0-expr->Expr expr))]
    [(? symbol? s) s]
    [(Float f) (Float f)]
    [(Int f) (Int f)]
    [other (error 'unsupported "Given : ~a" e)]))

(define (sym->temp-id [sym : Symbol]) : Id
  (Id sym 'UNDEFINED))

(define (lift-func-locals [body : (Listof Expr)]) : (Listof Symbol)
  (append-map
   (lambda ([e : Expr]) : (Listof Symbol)
     (match e
       [(LetVals ids vals body) (lift-let-locals ids vals body)]
       [(LetRecVals ids vals body) (lift-let-locals ids vals body)]
       [(Call fn args) (lift-func-locals args)]
       [(IndirectCall fn args) (lift-func-locals args)]
       [(If test t f) (append (lift-func-locals (list test))
                              (lift-func-locals (list t))
                              (lift-func-locals (list f)))]
       [(Begin exprs) (lift-func-locals exprs)]
       [(Begin0 exprs) (lift-func-locals exprs)]
       [(Set id expr) (lift-func-locals (list expr))]
       [(? symbol? s) '()]
       [other '()]))
   body))

(define (lift-let-locals [ids : (Listof (Listof Symbol))] [vals : (Listof Expr)] [body : (Listof Expr)]) : (Listof Symbol)
  (append
   (filter-map
    (lambda ([ids : (Listof Symbol)] [val : Expr])
      (first ids))
    ids
    vals)
   (lift-func-locals body)))

; Hashtable scoped by function names with a list of id's and their suspected types
(define-type TypeTable (HashTable Symbol (Listof Id)))
(define (discover-types [mod : Module]) : Module
  (let ((d-types (estimate-types (Module-funcs mod))))
    (Module (Module-exports mod)
            (filter-map (get-symbol-id d-types) (map Id-sym (Module-globals mod)))
            (map
             (lambda ([c : Closure])
               (Closure (Closure-name c)
                        (filter-map (get-symbol-id d-types) (map Id-sym (Closure-params c)))
                        (filter-map (get-symbol-id d-types) (map Id-sym (Closure-env-params c)))
                        (filter-map (get-symbol-id d-types) (map Id-sym (Closure-locals c)))
                        (Closure-body c)))
             (Module-funcs mod)))))

(define get-symbol-id : (-> TypeTable (-> Symbol (U #f Id)))
  (lambda ([d-types : TypeTable])
    (lambda ([s : Symbol]) : (U #f Id)
      (let ((res (findf (lambda ([id : Id]) (equal? s (Id-sym id))) (cast (flatten (hash-values d-types)) (Listof Id)))))
        (if (not res)
            (Id s 'UNKNOWN)
            res)))))

(define (estimate-types [funcs : (Listof Closure)]) : TypeTable
  (make-hash (map
              (lambda ([c : Closure])
                (cast (cons (Closure-name c) (estimate-closure-types c)) (Pairof Symbol (Listof Id))))
              funcs)))

(define (estimate-closure-types [clo : Closure]) : (Listof Id)
  (cast (flatten (map estimate-expr-types (Closure-body clo))) (Listof Id)))

; Returns a closure with the Id's filled in
(define (estimate-expr-types [e : Expr]) : (Listof Id)
  (match e
    [(LetVals ids vals body)
     (let ((f-ids (cast (flatten ids) (Listof Symbol)))
           (f-vals (map expr->type (cast (flatten vals) (Listof Expr)))))
       (append (map (lambda ([s : Symbol] [t : Type]) (Id s t)) f-ids f-vals)
               (append-map estimate-expr-types body)))]
    [(LetRecVals ids vals body)
     (let ((f-ids (cast (flatten ids) (Listof Symbol)))
           (f-vals (map expr->type (cast (flatten vals) (Listof Expr)))))
       (append (map (lambda ([s : Symbol] [t : Type]) (Id s t)) f-ids f-vals)
               (append-map estimate-expr-types body)))]
    [(Call fn args) (append-map estimate-expr-types args)]
    [(IndirectCall fn args) (append-map estimate-expr-types args)]
    [(If test t f) (append (estimate-expr-types test)
                           (estimate-expr-types t)
                           (estimate-expr-types f))]
    [(Begin exprs) (append-map estimate-expr-types exprs)]
    [(Begin0 exprs) (append-map estimate-expr-types exprs)]
    [(Set id expr) (let ((p-expr (estimate-expr-types expr)))
                     (cons (Id id (expr->type expr)) p-expr))]
    [other '()]))

(define (expr->type [e : Expr]) : Type
  (match e
    [(LetVals ids vals body) (expr->type (last body))]
    [(LetRecVals ids vals body) (expr->type (last body))]
    [(Call fn args) 'i32]
    [(IndirectCall fn args) 'i32]
    [(CaseLambda lams) 'i32]
    [(If test t f) (let ((t-type (expr->type t))
                         (f-type (expr->type f)))
                     (if (not (equal? t-type f-type))
                         (error 'typecheck "Error: If has mismatching branch types ~v ~v" t-type f-type)
                         f-type))]
    [(Begin exprs) (expr->type (last exprs))]
    [(Begin0 exprs) (expr->type (last exprs))]
    [(Set id expr) (expr->type expr)]
    ; If we encounter a symbol we know it's a function name
    [(? symbol? s) s]
    [(Int i) 'i64]
    [(Float f) 'f64]
    [other (error 'typecheck "Unknown Expression: ~v" e)]))

