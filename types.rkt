#lang typed/racket

(provide (all-defined-out))

(struct FEP ([provides : (Listof Provide)] [defs : (Listof TopDefinition)]) #:transparent)
(define-type TopDefinition (U Func Var))
(struct Var ([id : Id] [expr : Expr]) #:transparent)
(struct Func ([name : Id] [params : (Listof Id)] [body : (Listof Expr)]) #:transparent)

(struct Module ([exports : (Listof Provide)] [globals : (Listof Id)] [funcs : (Listof Closure)]) #:transparent)

(struct Closure ([name : Id] [params : (Listof Id)] [env-params : (Listof Id)] [locals : (Listof Id)] [body : (Listof Expr)]) #:transparent)

(define-type Expr (U Lam App CaseLambda If LetVals LetRecVals Begin Begin0 Set TopId Value))

(struct Lam ([params : (Listof Id)] [body : (Listof Expr)]) #:transparent)
(struct App ([func : (U Id Lam)] [args : (Listof Expr)]) #:transparent)
(struct CaseLambda ([funcs : (Listof Func)]) #:transparent)
(struct If ([test : Expr] [then : Expr] [else : Expr]) #:transparent)
(struct LetVals ([ids : (Listof (Listof Id))] [val-exprs : (Listof Expr)] [body : (Listof Expr)]) #:transparent)
(struct LetRecVals ([ids : (Listof (Listof Id))] [val-exprs : (Listof Expr)] [body : (Listof Expr)]) #:transparent)
(struct Begin ([exprs : (Listof Expr)]) #:transparent)
(struct Begin0 ([exprs : (Listof Expr)]) #:transparent)
(struct Set ([id : Id] [expr : Expr]) #:transparent)
(struct TopId ([id : Id]) #:transparent)

(define-type Value (U Id Float Int))

(struct Id ([sym : Symbol]) #:transparent)
(struct Float ([n : Real]) #:transparent)
(struct Int ([n : Integer]) #:transparent)

; Forms for providess
(define-type Provide (U SimpleProvide RenamedProvide AllDefined PrefixAllDefined))
(struct SimpleProvide ([id : Symbol]) #:transparent)
(struct RenamedProvide ([local-id : Symbol] [exported-id : Symbol]) #:transparent)
(struct AllDefined ([exclude : (Setof Symbol)]) #:transparent)
(struct PrefixAllDefined ([prefix-id : Symbol] [exclude : (Setof Symbol)]) #:transparent)

(define (Provide? [a : Any]) : Boolean
  (or (SimpleProvide? a) (RenamedProvide? a) (AllDefined? a) (PrefixAllDefined? a)))