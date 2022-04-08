#lang typed/racket

(require "types.rkt")

(provide full-pass)

(define-type Env (HashTable Symbol Symbol))

(define passes '(unique-variables))


(define (full-pass [ast : Program])
  (typecheck ast)
  (unique-variables ast)
  (lift-locals ast))

; Super simple typecheck but can elaborate later
; Juse make sure no top level definitions overshadow each other
(define (typecheck [ast : Program]) : Void
  (let ((dupes (check-duplicates (append
                                  (map (lambda ([fd : FuncDef]) (Id-sym (FuncDef-id fd))) (Program-funcs ast))
                                  (map (lambda ([vd : VarDef]) (Id-sym (VarDef-id vd))) (Program-globals ast))))))
    (when dupes (error 'typecheck (~a "ERROR: Overshadowing Globals: " dupes)))))

; We force globals to be unique, so just go through expressions and uniquify tgem
(define (unique-variables [ast : Program]) : Program
  (Program
   (Program-provides ast)
   (map (lambda ([vd : VarDef])
          (VarDef (VarDef-id vd) (uniquify-expr (VarDef-val vd) (make-hash))))
        (Program-globals ast))
   (map (lambda ([fd : FuncDef])
          (FuncDef (FuncDef-id fd)
                   (uniquify-func (FuncDef-func fd))))
        (Program-funcs ast))))

(define (uniquify-func [func : Func]) : Func
  (let ((new-args (map (lambda ([id1 : Id] [id2 : Id]) (cons (Id-sym id1) (gensym (Id-sym id2))))
                       (Func-args func)
                       (Func-args func))))
    (Func (map (lambda ([id-pair : (Pairof Symbol Symbol)]) (Id (cdr id-pair))) new-args)
          (Func-locals func)
          (map (lambda ([expr : Expr]) (uniquify-expr expr (make-hash new-args))) (Func-body func)))))


(define (uniquify-expr [expr : Expr] [env : Env]) : Expr
  (match expr
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

(define (lift-locals [ast : Program]) : Program
  (Program
   (Program-provides ast)
   (Program-globals ast)
   (map (lambda ([fd : FuncDef])
          (FuncDef (FuncDef-id fd)
                   (lift-func-locals (FuncDef-func fd))))
        (Program-funcs ast))))

(define (lift-func-locals [f : Func]) : Func
  (Func (Func-args f)
        (get-local-syms (Func-body f))
        (Func-body f)))

(define (get-local-syms [body : (Listof Expr)]) : (Listof Id)
  (append-map
   (lambda ([e : Expr]) : (Listof Id)
     (match e
       [(LetVals ids vals body) (cast (flatten ids) (Listof Id))]
       [(LetRecVals ids vals body) (cast (flatten ids) (Listof Id))]
       [other '()]))
   body))

(define (uniquify-id [s : Symbol] [env : Env]) : Id
  (Id (if (hash-has-key? env s) (hash-ref env s) s)))
