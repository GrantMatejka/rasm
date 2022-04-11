#lang typed/racket

(require "types.rkt")

(provide full-pass)

(define-type Env (HashTable Symbol Symbol))

(define passes '(unique-variables))

; TODO: We need to lift all global declarations to only be constant expressions and then initialize them, expressions specifically

(define (full-pass [ast : Program])
  (typecheck ast)
  (lift-locals (unique-variables ast)))

; Super simple typecheck but can elaborate later
; Juse make sure no top level definitions overshadow each other
(define (typecheck [ast : Program]) : Void
  (let ((dupes (check-duplicates (append
                                  (map (lambda ([f : Func]) (Id-sym (Func-name f))) (Program-funcs ast))
                                  (map (lambda ([vd : Var]) (Id-sym (Var-id vd))) (Program-globals ast))))))
    (when dupes (error 'typecheck (~a "ERROR: Overshadowing Globals: " dupes)))))

; We force globals to be unique, so just go through expressions and uniquify tgem
(define (unique-variables [ast : Program]) : Program
  (Program
   (Program-provides ast)
   (map (lambda ([vd : Var])
          (Var (Var-id vd) (uniquify-expr (Var-expr vd) (make-hash))))
        (Program-globals ast))
   (map uniquify-func (Program-funcs ast))))

(define (uniquify-func [func : Func]) : Func
  (let ((new-args (map (lambda ([id1 : Id] [id2 : Id]) (cons (Id-sym id1) (gensym (Id-sym id2))))
                       (Func-params func)
                       (Func-params func))))
    (Func (Func-name func)
          (map (lambda ([id-pair : (Pairof Symbol Symbol)]) (Id (cdr id-pair))) new-args)
          (Func-locals func)
          (map (lambda ([expr : Expr]) (uniquify-expr expr (make-hash new-args))) (Func-body func)))))


(define (uniquify-expr [expr : Expr] [env : Env]) : Expr
  (match expr
    ; TODO: I think letvals and letrecvals are handled the same for us, can we combine these???
    [(LetVals ids vals body) (let ((new-ids (map (lambda ([l-id : (Listof Id)])
                                                   (map (lambda ([id1 : Id] [id2 : Id])
                                                          (let ((p-ids (cons (Id-sym id1) (gensym (Id-sym id2)))))
                                                            (hash-set! env (car p-ids) (cdr p-ids))
                                                            (Id (cdr p-ids))))
                                                        l-id
                                                        l-id))
                                                 ids)))
                               (LetVals new-ids
                                        (map (lambda ([e : Expr]) (uniquify-expr e env)) vals)
                                        (map (lambda ([e : Expr]) (uniquify-expr e env)) body)))]
    [(LetRecVals ids vals body) (let ((new-ids (map (lambda ([l-id : (Listof Id)])
                                                      (map (lambda ([id1 : Id] [id2 : Id])
                                                             (let ((p-ids (cons (Id-sym id1) (gensym (Id-sym id2)))))
                                                               (hash-set! env (car p-ids) (cdr p-ids))
                                                               (Id (cdr p-ids))))
                                                           l-id
                                                           l-id))
                                                    ids)))
                                  (LetRecVals new-ids
                                              (map (lambda ([e : Expr]) (uniquify-expr e env)) vals)
                                              (map (lambda ([e : Expr]) (uniquify-expr e env)) body)))]
    [(App fn args) (App (uniquify-id (Id-sym fn) env) (map (lambda ([e : Expr]) (uniquify-expr e env)) args))]
    [(CaseLambda funcs) (CaseLambda (map uniquify-func funcs))]
    [(If test t f) (If (uniquify-expr test env) (uniquify-expr t env) (uniquify-expr f env))]
    [(Begin exprs) (Begin (map (lambda ([e : Expr]) (uniquify-expr e env)) exprs))]
    [(Begin0 exprs) (Begin0 (map (lambda ([e : Expr]) (uniquify-expr e env)) exprs))]
    [(Set id expr) (Set (uniquify-id (Id-sym id) env) (uniquify-expr expr env))]
    [(Id s) (uniquify-id s env)]
    [other expr]))

; We need to know all the local variables of a function so we can declare them at the beginning of the function
(define (lift-locals [ast : Program]) : Program
  (Program
   (Program-provides ast)
   (Program-globals ast)
   (map (lambda ([f : Func])
          (Func (Func-name f)
                (Func-params f)
                (lift-func-locals (Func-body f))
                (Func-body f)))
        (Program-funcs ast))))

(define (lift-func-locals [body : (Listof Expr)]) : (Listof Id)
  (append-map
   (lambda ([e : Expr]) : (Listof Id)
     (match e
       [(LetVals ids vals body) (append (cast (flatten ids) (Listof Id)) (lift-func-locals body))]
       [(LetRecVals ids vals body) (append (cast (flatten ids) (Listof Id)) (lift-func-locals body))]
       [(App fn args) (lift-func-locals args)]
       [(If test t f) (append (lift-func-locals (list test)) (lift-func-locals (list t)) (lift-func-locals (list f)))]
       [(Begin exprs) (lift-func-locals exprs)]
       [(Begin0 exprs) (lift-func-locals exprs)]
       [(Set id expr) (lift-func-locals (list expr))]
       [(Id s) '()]
       [other '()]))
   body))

(define (uniquify-id [s : Symbol] [env : Env]) : Id
  (Id (if (hash-has-key? env s) (hash-ref env s) s)))



