#lang racket

(require "types.rkt")

(provide build-wat)

(define prims (hash
               '+ 'f64.add
               '- 'f64.sub
               '* 'f64.mul
               '/ 'f64.div
               '< 'f64.lt
               'equal? 'f64.eq))

(define need-init '())

(define (build-wat ast)
  `(module
       ,@(filter-map process-tdef (Program-globals ast))
     ,@(filter-map process-tdef (Program-funcs ast))
     (func $init ,@(filter-map build-init need-init))
     (start $init)))

(define (process-tdef def)
  (match def
    ; TODO: This is a type hack
    [(VarDef (Id id) (Int n)) `(global ,(wat-name id) (mut f64) (f64.const ,n))]
    [(VarDef (Id id) (Float n)) `(global ,(wat-name id) (mut f64) (f64.const ,n))]
    [(VarDef (Id id) (App f args)) (set! need-init (cons def need-init))
                                          `(global ,(wat-name id) (mut f64) (f64.const 0))]
    [(FuncDef (Id fn) (Func params body))
     `(func ,(wat-name fn)
            (export ,(~a '\" fn '\"))
            ,@(map (lambda (p) `(param ,(wat-name p) f64)) params)
            (result f64)
            ,@(map process-expr body))]
    [other (error 'unsupported (~a def))]))

(define (process-expr body)
  (match body
    [(App (Id fn) args)
     (if (hash-has-key? prims fn)
         `(,(hash-ref prims fn) ,@(map process-expr args))
         `(call ,(wat-name fn) ,@(map process-expr args)))]
    [(If test t f)
     `(if (result f64) ,@(block test)
          (then ,@(block t))
          (else ,@(block f)))]
    ; TODO: Actually handle ids and vals
    [(LetVals ids vals exprs)
     (map process-expr exprs)]
    [(Begin exprs)
     (map process-expr exprs)]
    ; TODO: Differentiate between local & global vars
    [(Id id) `(local.get ,(wat-name id))]
    [(Float n) `(f64.const ,n)]
    [(Int n) `(f64.const ,n)]
    [other (error 'unsupported (~a body))]))

; TODO: We need to use this if we may have a list of statements
(define (block bl)
  (let ((p-bl (process-expr bl)))
    (if (decon? p-bl) p-bl (list p-bl))))

(define (build-init stmt)
  (match stmt
    [(TopDef (list (Id id)) (App f args))
     `(global.set ,(wat-name id) ,(process-expr (TopDef-val stmt)))]
    [other #f]))

(define (wat-name id)
  (string->symbol
   (~a "$" 
       (match id
         [(Id s) s]
         [(? symbol? id) id]
         [other (error 'unsupported id)]))))


; Tells us if we need to deconstruct or not
(define (decon? src)
  (and (list? src) (andmap list? src)))
