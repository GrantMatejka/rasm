#lang racket

(provide (all-defined-out))

;(struct Func ([args : (Listof Id)] [body : (Listof (U Stmt Expr))]) #:transparent)
(struct Func (args body) #:transparent)
;(struct TopDef ([ids : (Listof Id)] [val : TopVal]) #:transparent)
(struct TopDef (ids val) #:transparent)
;(struct App ([func : Id] [args : (Listof Expr)]) #:transparent)
(struct App (func args) #:transparent)
;(struct Id ([sym : Symbol]) #:transparent)
(struct Id (sym) #:transparent)
;(struct Num ([n : Number]) #:transparent)
(struct Num (n) #:transparent)

