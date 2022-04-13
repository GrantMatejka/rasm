#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide build-wat)

(define prims (hash
               '+ 'f64.add
               '- 'f64.sub
               '* 'f64.mul
               '/ 'f64.div
               '< 'f64.lt
               'equal? 'f64.eq))

(define-type FuncTable (HashTable Symbol Symbol))
(define env : FuncTable (make-hash))

(define need-init : (Listof TopDefinition) '())

; TODO: We shopuld build the initialization function in one of the passes and then we can lift the locals and everything like we're supposed to
(define (build-wat [mod : Module]) : Sexp
  `(module
       ,@(map (lambda ([g : Id]) (declare-globals mod g)) (Module-globals mod))
     ,@(filter-map (lambda ([c : Closure]) (process-topdef mod c)) (Module-funcs mod))
     (start $init)))

(define (declare-globals [mod : Module] [global : Id]) : Sexp
  `(global ,(wat-name global)
           ,@(if (findf (lambda ([p : Provide]) (and (SimpleProvide? p) (equal? (SimpleProvide-id p) (Id-sym global)))) (Module-exports mod)) (list `(export ,(~a '\" (Id-sym global) '\"))) '())
           (mut f64)
           (f64.const 0)))

(define (process-topdef [mod : Module] [clo : Closure]) : Sexp
  (let ((globals (Module-globals mod))
        (exports (Module-exports mod))
        (funcs (Module-funcs mod)))
    (match clo
      [(Closure (Id fn) params env-params locals body)
       `(func ,(wat-name fn)
              ; TODO: Actually handle exports, but low priority
              ,@(if (findf (lambda ([p : Provide]) (and (SimpleProvide? p) (equal? (SimpleProvide-id p) fn))) exports) (list `(export ,(~a '\" fn '\"))) '())
              ,@(map (lambda ([p : (U Id Symbol)]) `(param ,(wat-name p) f64)) (append params env-params))
              ,@(if (equal? fn 'init) '() '((result f64)))
              ,@(map (lambda ([l : (U Id Symbol)]) `(local ,(wat-name l) f64)) locals)
              ,@(append-map (lambda ([e : Expr]) (process-expr globals locals funcs e)) body))]
      [other (error 'unsupported (~a clo))])))

(define (process-expr [globals : (Listof Id)] [locals : (Listof Id)] [funcs : (Listof Closure)] [expr : Expr]) : (Listof Sexp)
  (match expr
    [(App (Id fn) args)
     (let ((func (findf (lambda ([c : Closure]) (equal? fn (Id-sym (Closure-name c)))) funcs)))
         (list (if (hash-has-key? prims fn)
               `(,(hash-ref prims fn) ,@(append-map (lambda ([e : Expr]) (process-expr globals locals funcs e)) args))
               `(call ,(wat-name (if (hash-has-key? env fn) (hash-ref env fn) fn))
                      ,@(append-map (lambda ([e : Expr]) (process-expr globals locals funcs e)) args)
                      ; We know args will handle whatever arguments to pass, so any leftover params are env vars to pass in
                      ,@(if func
                            (append-map (lambda ([i : Id]) (process-expr globals locals funcs i)) (Closure-env-params func))
                            '())))))]
    [(If test t f)
     (list `(if (result f64) ,@(process-expr globals locals funcs test)
                (then ,@(process-expr globals locals funcs t))
                (else ,@(process-expr globals locals funcs f))))]
    ; TODO: Actually handle ids and vals
    [(LetVals ids vals exprs)
     (append
      (filter-map
       ; TODO: HAck around not actually supporting multiple return vals
       (lambda ([l-id : (Listof Id)] [val : Expr])
         (cond
           ; We know we have a funtion to call
           [(Id? val) (hash-set! env (Id-sym (first l-id)) (Id-sym val)) #f]
           [else `(local.set ,(wat-name (first l-id)) ,@(process-expr globals locals funcs val))]))
       ids
       vals)
      (append-map (lambda ([e : Expr]) (process-expr globals locals funcs e)) exprs))]
    [(Begin exprs)
     (append-map (lambda ([e : Expr]) (process-expr globals locals funcs e)) exprs)]
    [(Set (Id id) expr) (if (findf (lambda ([i : Id]) (equal? (Id-sym i) id)) globals)
                            (list `(global.set ,(wat-name id) ,@(process-expr globals locals funcs expr)))
                            (list `(local.set ,(wat-name id) ,@(process-expr globals locals funcs expr))))]
    ; TODO: Differentiate between local & global vars
    [(Id id) (if (findf (lambda ([i : Id]) (equal? (Id-sym i) id)) globals)
                 (list `(global.get ,(wat-name id)))
                 (list `(local.get ,(wat-name id))))]
    [(Float n) (list `(f64.const ,n))]
    [(Int n) (list `(f64.const ,n))]
    [other (error 'unsupported (~a expr))]))

(define (wat-name [id : (U Id Symbol)]) : Symbol
  (string->symbol
   (~a "$" 
       (match id
         [(Id s) s]
         [(? symbol? id) id]
         [other (error 'unsupported (~a id))]))))


