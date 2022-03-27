#lang racket

(require "types.rkt")

(provide build-wat)

(define prims (hash
               '+ 'f64.add
               '- 'f64.sub
               '* 'f64.mul
               '/ 'f64.div))

(define need-init '())

(define (build-wat ast)
  `(module
       ,@(filter-map process-tdef ast)
     (func $init ,@(filter-map build-init need-init))
     (start $init)))

(define (process-tdef def)
  (match def
    ; TODO: This is hardcode type hack
    [(TopDef (list (Id id)) (Num n)) `(global ,(wat-name id) (mut f64) (f64.const ,n))]
    [(TopDef (list (Id id)) (App f args)) (set! need-init (cons def need-init))
                                          `(global ,(wat-name id) (mut f64) (f64.const 0))]
    [(TopDef (list (Id fn)) (Func params body))
     `(func ,(wat-name fn)
            (export ,(~a '\" fn '\"))
            ,@(map (lambda (p) `(param ,(wat-name p) f64)) params)
            (result f64)
            ,@(map process-expr body))]
    [other (error 'unsupported)]))

(define (process-expr body)
  (match body
    [(App (Id fn) args)
     (if (hash-has-key? prims fn)
         `(,(hash-ref prims fn) ,@(map process-expr args))
         `(call ,(wat-name fn) ,@(map process-expr args)))]
    ; TODO: Differentiate between local & global vars
    [(Id id) `(local.get ,(wat-name id))]
    [(Num n) `(f64.const ,n)]
    [other (error 'unsupported)]))

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
