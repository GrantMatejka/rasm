#lang typed/racket

(require "types.rkt")

(provide build-wat)

(define prims (hash
               '+ 'f64.add
               '- 'f64.sub
               '* 'f64.mul
               '/ 'f64.div
               '< 'f64.lt
               'equal? 'f64.eq))

(define need-init : (Listof TopDefinition) '())

(define (build-wat [ast : Program]) : Sexp
  `(module
       ,@(filter-map process-topdef (Program-globals ast))
     ,@(filter-map process-topdef (Program-funcs ast))
     (func $init ,@(filter-map build-init need-init))
     (start $init)))

(define (process-topdef [def : TopDefinition]) : Sexp
  (match def
    ; TODO: All types are hacks
    ; We specifically need to handle applications so we can initialize the values later
    ; This is necessary as global initializer expression must be a constant expression
    [(Var (Id id) (App f args)) (begin
                                     (set! need-init (cons def need-init))
                                     `(global ,(wat-name id) (mut f64) (f64.const 0)))]
    [(Var (Id id) expr) `(global ,(wat-name id) (mut f64) ,@(process-expr expr))]
    [(Func (Id fn) params locals body)
     `(func ,(wat-name fn)
            ; HACK: Is this really necessary, also a hack bc we export everything that isn't a an anonymous function
            ,@(if (anon? fn) '() (list `(export ,(~a '\" fn '\"))))
            ,@(map (lambda ([p : (U Id Symbol)]) `(param ,(wat-name p) f64)) params)
            (result f64)
            ,@(map (lambda ([l : (U Id Symbol)]) `(local ,(wat-name l) f64)) locals)
            ,@(append-map process-expr body))]
    [other (error 'unsupported (~a def))]))

(define (process-expr [expr : Expr]) : (Listof Sexp)
  (match expr
    [(App (Id fn) args)
     (list (if (hash-has-key? prims fn)
               `(,(hash-ref prims fn) ,@(append-map process-expr args))
               `(call ,(wat-name fn) ,@(append-map process-expr args))))]
    [(If test t f)
     (list `(if (result f64) ,@(process-expr test)
                (then ,@(process-expr t))
                (else ,@(process-expr f))))]
    ; TODO: Actually handle ids and vals
    [(LetVals ids vals exprs)
     (append
      (map
       ; TODO: HAck around not actually supporting multiple return vals
       (lambda ([l-id : (Listof Id)] [val : Expr]) `(local.set ,(wat-name (first l-id)) ,@(process-expr val)))
       ids
       vals)
      (append-map process-expr exprs))]
    [(Begin exprs)
     (append-map process-expr exprs)]
    ; TODO: Differentiate between local & global vars
    [(Id id) (list `(local.get ,(wat-name id)))]
    [(Float n) (list `(f64.const ,n))]
    [(Int n) (list `(f64.const ,n))]
    [other (error 'unsupported (~a expr))]))

(define (build-init [def : TopDefinition]) : (U Sexp #f)
  (match def
    [(Var (list (Id id)) (App f args))
     `(global.set ,(wat-name id) ,(process-expr (Var-expr def)))]
    [other #f]))

(define (wat-name [id : (U Id Symbol)]) : Symbol
  (string->symbol
   (~a "$" 
       (match id
         [(Id s) s]
         [(? symbol? id) id]
         [other (error 'unsupported (~a id))]))))


; Whether we have an anonymous lambda or not
(define (anon? [fn : Symbol]) : Boolean
  (string-contains? (~a fn) "__lambda"))


