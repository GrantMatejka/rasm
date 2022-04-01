#lang typed/racket

(provide (all-defined-out))

(struct Program ([globals : (Listof VarDef)] [funcs : (Listof FuncDef)]) #:transparent)

(struct FuncDef ([id : Id] [func : Func]) #:transparent)
(struct VarDef ([id : Id] [val : Expr]) #:transparent)

; USed for generating our AST
(define-type TopVal (U Func Expr))
(struct TopDef ([ids : Id] [val : TopVal]) #:transparent)

; TODO: We need a way to identify lambdas???
(struct Func ([args : (Listof Id)] [body : (Listof Expr)]) #:transparent)

(define-type Expr (U App If LetVals Begin Value))

(struct App ([func : (U Id Func)] [args : (Listof Expr)]) #:transparent)
(struct If ([test : Expr] [then : Expr] [else : Expr]) #:transparent)
(struct LetVals ([ids : (Listof Id)] [val-exprs : (Listof Expr)] [body : (Listof Expr)]) #:transparent)
(struct Begin ([exprs : (Listof Expr)]) #:transparent)

(define-type Value (U Id Float Int))

(struct Id ([sym : Symbol]) #:transparent)
(struct Float ([n : Real]) #:transparent)
(struct Int ([n : Integer]) #:transparent)

