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
       ,@(filter-map process-topdef (Program-globals ast))
     ,@(filter-map process-topdef (Program-funcs ast))
     (func $init ,@(filter-map build-init need-init))
     (start $init)))

(define (process-topdef def)
  (match def
    ; TODO: This is a type hack
    [(VarDef (Id id) (Int n)) `(global ,(wat-name id) (mut f64) (f64.const ,n))]
    [(VarDef (Id id) (Float n)) `(global ,(wat-name id) (mut f64) (f64.const ,n))]
    [(VarDef (Id id) (App f args)) (set! need-init (cons def need-init))
                                          `(global ,(wat-name id) (mut f64) (f64.const 0))]
    [(FuncDef (Id fn) (Func params locals body))
     `(func ,(wat-name fn)
            ; HACK: Is this really necessary, also a hack bc we export everything that isn't a an anonymous function
            ,@(if (anon? fn) '() (list `(export ,(~a '\" fn '\"))))
            ,@(map (lambda (p) `(param ,(wat-name p) f64)) params)
            (result f64)
            ,@(map (lambda (l) `(local ,(wat-name l) f64)) locals)
            ,@(append-map process-expr body))]
    [other (error 'unsupported (~a def))]))

(define (process-expr expr)
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
       (lambda (id val) `(local.set ,(wat-name (first id)) ,@(process-expr val)))
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


; Whether we have an anonymous lambda or not
(define (anon? fn)
  (string-contains? (~a fn) "lambda"))

; NEEDED???
; TODO: We need to use this if we may have a list of statements
(define (block bl)
  (let ((p-bl (process-expr bl)))
    (if (decon? p-bl) p-bl (list p-bl))))

; Tells us if we need to deconstruct or not
(define (decon? src)
  (and (list? src) (andmap list? src)))
