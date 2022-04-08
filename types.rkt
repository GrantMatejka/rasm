#lang typed/racket

(provide (all-defined-out))

(struct Program ([provides : Provide*] [globals : (Listof VarDef)] [funcs : (Listof FuncDef)]) #:transparent)

(struct FuncDef ([id : Id] [func : Func]) #:transparent)
(struct VarDef ([id : Id] [val : Expr]) #:transparent)

; Used for generating our AST
(define-type TopVal (U Func Expr))

; TODO: Support multiple return vals
(struct TopDef ([id : Id] [val : TopVal]) #:transparent)

(struct Func ([args : (Listof Id)] [locals : (Listof Id)] [body : (Listof Expr)]) #:transparent)

(define-type Expr (U App CaseLambda If LetVals LetRecVals Begin Begin0 Set TopId Value))

(struct App ([func : Id] [args : (Listof Expr)]) #:transparent)
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

; Forms for provide

(struct Provide* ([ps : (Listof Provide)]) #:transparent)
(define-type Provide (U SimpleProvide RenamedProvide AllDefined PrefixAllDefined))
(struct SimpleProvide ([id : Symbol]) #:transparent)
(struct RenamedProvide ([local-id : Symbol] [exported-id : Symbol]) #:transparent)
(struct AllDefined ([exclude : (Setof Symbol)]) #:transparent)
(struct PrefixAllDefined ([prefix-id : Symbol] [exclude : (Setof Symbol)]) #:transparent)

