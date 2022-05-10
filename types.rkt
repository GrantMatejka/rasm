#lang typed/racket

(provide (all-defined-out))

#|
  Mock Nanopass Framework, with only one IR
|#

; Represents fully expanded racket program
(struct FEP ([provides : (Listof Provide)] [defs : (Listof TopDefinition)]) #:transparent)
; Represents a temporary stage of fully expanded programs where we introduce expressions used to initialize global values
(struct FEP2 FEP ([init : (Listof L0-Expr)]) #:transparent)

; Type definitions for global values, where a global is either a function definiton or a variable
(define-type TopDefinition (U Func Var))
(struct Var ([id : Symbol] [expr : L0-Expr]) #:transparent)
(struct Func ([name : Symbol] [params : (Listof Symbol)] [env : (Listof Symbol)] [body : (Listof L0-Expr)]) #:transparent)

; Intermediate Expression Representation
(define-type L0-Expr (U L0-Lam L0-App L0-CaseLambda L0-If L0-LetVals L0-LetRecVals L0-Begin L0-Begin0 L0-Set L0-TopId L0-Value))
(struct L0-App ([func : L0-Expr] [args : (Listof L0-Expr)]) #:transparent)
(struct L0-Lam ([params : (Listof Symbol)] [body : (Listof L0-Expr)]) #:transparent)
(struct L0-CaseLambda ([funcs : (Listof L0-Expr)]) #:transparent)
(struct L0-If ([test : L0-Expr] [then : L0-Expr] [else : L0-Expr]) #:transparent)
(struct L0-LetVals ([ids : (Listof (Listof Symbol))] [val-exprs : (Listof L0-Expr)] [body : (Listof L0-Expr)]) #:transparent)
(struct L0-LetRecVals ([ids : (Listof (Listof Symbol))] [val-exprs : (Listof L0-Expr)] [body : (Listof L0-Expr)]) #:transparent)
(struct L0-Begin ([exprs : (Listof L0-Expr)]) #:transparent)
(struct L0-Begin0 ([exprs : (Listof L0-Expr)]) #:transparent)
(struct L0-Set ([id : Symbol] [expr : L0-Expr]) #:transparent)
(struct L0-TopId ([id : Symbol]) #:transparent)

(define-type L0-Value (U Symbol Float Int))
(struct L0-Float ([n : Real]) #:transparent)
(struct L0-Int ([n : Integer]) #:transparent)


; Final types we are expecting
(define-type Env (HashTable Symbol Symbol))

(struct Module ([exports : (Listof Provide)] [globals : (Listof Id)] [funcs : (Listof Closure)]) #:transparent)
(struct Closure ([name : Symbol]
                 [params : (Listof Id)]
                 [env-params : (Listof Id)]
                 [locals : (Listof Id)]
                 [body : (Listof Expr)]) #:transparent)

(struct Id ([sym : Symbol] [type : Type]) #:transparent)

(define-type Type (U Symbol 'i32 'i64 'f64))

(define-type Expr (U Call IndirectCall CaseLambda If LetVals LetRecVals Begin Begin0 Set TopId Value))

; We only call primitive functions directly
(struct Call ([func : Symbol] [args : (Listof Expr)]) #:transparent)
(struct IndirectCall ([func : Symbol] [args : (Listof Expr)]) #:transparent)

(struct CaseLambda ([funcs : (Listof Expr)]) #:transparent)
(struct If ([test : Expr] [then : Expr] [else : Expr]) #:transparent)
(struct LetVals ([ids : (Listof (Listof Symbol))] [val-exprs : (Listof Expr)] [body : (Listof Expr)]) #:transparent)
(struct LetRecVals ([ids : (Listof (Listof Symbol))] [val-exprs : (Listof Expr)] [body : (Listof Expr)]) #:transparent)
(struct Begin ([exprs : (Listof Expr)]) #:transparent)
(struct Begin0 ([exprs : (Listof Expr)]) #:transparent)
(struct Set ([id : Symbol] [expr : Expr]) #:transparent)
(struct TopId ([id : Symbol]) #:transparent)

(define-type Value (U Symbol Float Int))
(struct Float ([n : Real]) #:transparent)
(struct Int ([n : Integer]) #:transparent)

; Forms for provides
; Borrowed from racketscript @ https://github.com/racketscript/racketscript/blob/master/racketscript-compiler/racketscript/compiler/expand.rkt
(define-type Provide (U SimpleProvide RenamedProvide AllDefined PrefixAllDefined))
(struct SimpleProvide ([id : Symbol]) #:transparent)
(struct RenamedProvide ([local-id : Symbol] [exported-id : Symbol]) #:transparent)
(struct AllDefined ([exclude : (Setof Symbol)]) #:transparent)
(struct PrefixAllDefined ([prefix-id : Symbol] [exclude : (Setof Symbol)]) #:transparent)

