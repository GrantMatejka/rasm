#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide build-wat)

; TODO: Get loop.rkt working

; Maps function name to wasm table index
(define-type FuncTable (HashTable Symbol Integer))
(define func-table : FuncTable (make-hash))

(define header `((\; "This gives us 256 pages of memory where each page is 64KiB" \;)
                 (memory $mem 256)
                 (export \"memory\" (memory $mem))
                 (global $__mem_head (mut i32) (i32.const 1))))

(define imports `((import \"env\" \"log_i32\" (func $log_i32 (param i32)))
                  (import \"env\" \"log_i64\" (func $log_i64 (param i64)))
                  (import \"env\" \"log_f64\" (func $log_f64 (param f64)))))

(define (build-wat [mod : Module]) : Sexp
  (hash-clear! func-table)
  `(module
       ,@imports
     ,@header
     (\; ------------------------------ \;)
     (\; ------------------------------ \;)
     (\; ------------start------------- \;)
     (\; ------------------------------ \;)
     (\; ------------------------------ \;)
     ,@(build-functable mod)
     ,@(export-func-num-param mod)
     (type $__function_type (func (param i32) (param i32) (result i32)))
     ,@(build-globals mod)
     ,@(build-funcs mod)
     (start $init)
     (\; ------------------------------ \;)
     (\; ------------------------------ \;)
     (\; -------------end-------------- \;)
     (\; ------------------------------ \;)
     (\; ------------------------------ \;)
     ,@wat-stdlib))

(define (build-functable [mod : Module]) : (Listof Sexp)
  (let ((funcs (filter (lambda ([c : Closure]) (not (equal? (Closure-name c) 'init))) (Module-funcs mod))))
    `((table ,(length funcs) funcref)
      (elem (i32.const 0)
            ,@(map (lambda ([clo : Closure] [idx : Real])
                     (hash-set! func-table (Closure-name clo) (real->int idx))
                     (wat-name (Closure-name clo)))
                   funcs
                   (range (length funcs)))))))

(define (export-func-num-param [mod : Module]) : (Listof Sexp)
  (let ((funcs (filter (lambda ([c : Closure]) (not (equal? (Closure-name c) 'init))) (Module-funcs mod))))
    (map (lambda ([clo : Closure]) `(global (export ,(~v (~a "__" (Closure-name clo) "_length"))) i32 (i32.const ,(length (Closure-params clo))))) funcs)))

(define (build-globals [mod : Module]) : (Listof Sexp)
  (map (lambda ([global : Symbol])
         `(global ,(wat-name global)
                  ,@(export? global (Module-exports mod))
                  (mut i32)
                  (i32.const 0)))
       (map Id-sym (Module-globals mod))))

(define (build-funcs [mod : Module]) : (Listof Sexp)
  (filter-map (lambda ([c : Closure]) (process-topdef mod c)) (Module-funcs mod)))

(define (process-topdef [mod : Module] [clo : Closure]) : Sexp
  (let ((globals (Module-globals mod))
        (exports (Module-exports mod))
        (funcs (Module-funcs mod)))
    ; At this point the only top level/non-global variable definitions we have are the functions themselves
    (match clo
      [(Closure fn params env-params locals body)
       `(func ,(wat-name fn)
              ,@(export? fn exports)
              ,@(if (equal? fn 'init) '() (list '(param $param_list i32) '(param $env_list i32)))
              ;,@(wat-params (map Id-sym (append params env-params)))
              ,@(if (equal? fn 'init) '() (list `(result i32)))
              ,@(wat-locals (map Id-sym (append locals params env-params)))
              (local $__app_local_arr_ptr i32)
              ,@(map
                 (lambda ([id : Id] [r : Real])
                   `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $param_list)))))
                 (append params env-params)
                 (range (length (append params env-params))))
              (local.set $__app_local_arr_ptr (i32.const 0))
              ,@(process-func-body globals env-params (append params locals) funcs body))]
      [other (error 'unsupported (~a clo))])))

(define (process-func-body [globals : (Listof Id)]
                           [env-params : (Listof Id)]
                           [locals : (Listof Id)]
                           [funcs : (Listof Closure)]
                           [body : (Listof Expr)]) : (Listof Sexp)
  (append-map (lambda ([e : Expr]) (process-expr globals env-params locals funcs (make-hash) e)) body))

(define (process-expr [globals : (Listof Id)]
                      [env-params : (Listof Id)]
                      [locals : (Listof Id)]
                      [funcs : (Listof Closure)]
                      [env : Env]
                      [expr : Expr]) : (Listof Sexp)
  (define global? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) globals)))
  (define local? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) locals)))
  (define env? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) env-params)))
  (define pe-helper (lambda ([env : Env]) (lambda ([e : Expr]) (process-expr globals env-params locals funcs env e))))
  (match expr
    [(App fn args)
     (let ((p-fn (hash-ref env fn (lambda () fn))))
       (if (hash-has-key? prims p-fn)
           ; If we have a primitive we can call it directly
           (call-primitive p-fn (append-map (pe-helper env) args))
           ; Otherwise we need to call the function indirectly
           (letrec ((funcname (let ((var? (findf (lambda ([id : Id]) (equal? (Id-sym id) p-fn)) (append globals env-params locals)))
                                 (static? (findf (lambda ([clo : Closure]) (equal? (Closure-name clo) p-fn)) funcs)))
                             (if var? (Id-type var?)
                                 (if static? (Closure-name static?) (error 'application "Unknown Function: ~a" fn)))))
                 (pp-fn ((pe-helper env) p-fn)))
             `((\; Set up parameters for indirect call \;)
               (local.set $__app_local_arr_ptr (call $__alloc (i32.const ,(* 4 (length args)))))
               ,@(map
                  (lambda ([src : Sexp] [r : Real])
                    `(i32.store (i32.add (i32.const ,(* (real->int r) 4)) (local.get $__app_local_arr_ptr)) ,src))
                  (append-map (pe-helper env) args
                          )
                  (range (length args)))
               (\; Pointer to arguments for function call \;)
               (local.get $__app_local_arr_ptr)
               (\; Pointer to env of the function we\'re calling \;)
               (i32.load (i32.add (i32.const 5) ,@pp-fn))
               (call_indirect (type $__function_type)
                              ,(if (hash-has-key? func-table fn)
                                   `(i32.const ,(hash-ref func-table fn))
                                   `(i32.load (i32.add (i32.const 1) ,@pp-fn))))))))]
    [(If test t f)
     (list `(if (result i32) (i64.eqz (i64.load (i32.add (i32.const 1) ,@((pe-helper env) test))))
                ; Since we are checking the conditional for eqz we need to reverse the conditions
                (then ,@((pe-helper env) f))
                (else ,@((pe-helper env) t))))]
    ; TODO: Actually handle ids and vals
    [(LetVals ids vals exprs)
     (append
      (filter-map
       ; TODO: HAck around not actually supporting multiple return vals
       (lambda ([l-id : (Listof Symbol)] [val : Expr])
         (cond
           ; We know we have a function to call
           [(symbol? val) (hash-set! env (first l-id) val) #f]
           [else `(local.set ,(wat-name (first l-id)) ,@((pe-helper env) val))]))
       ids
       vals)
      (append-map (pe-helper env) exprs))]
    [(Begin exprs)
     (append-map (pe-helper env) exprs)]
    ; TODO: For set, should w ebe changing the pointer or the value at the ptr???
    [(Set id expr) (let ((p-expr ((pe-helper env) expr)))
                     (if (findf (lambda ([i : Symbol]) (equal? i id)) (map Id-sym globals))
                         (list `(global.set ,(wat-name id) ,@p-expr))
                         (list `(local.set ,(wat-name id) ,@p-expr))))]
    [(TopId id) (if (global? id)
                    (list `(global.get ,(wat-name id)))
                    (error 'top-id "Cannot find expected global id ~a" id))]
    ; If we get here, we want to retrieve the value of the specified id
    [(? symbol? id) (let ((func (findf (lambda ([f : Closure]) (equal? id (Closure-name f))) funcs)))
                       (cond
                         [(local? id) (list `(local.get ,(wat-name id)))]
                         
                         ; TODO : If we implement list ref this should work??? bc every function will know where it expects the env params to be
                         [(env? id) (list `(call $__list_ref
                                                 (local.get $env_list)
                                                 (i32.const ,(index-of env-params id (lambda ([env-id : Id] [id : Symbol]) (equal? (Id-sym env-id) id))))))]
                         [(global? id) (list `(global.get ,(wat-name id)))]
                         ; If we ever encounter a function in this instance, it's not being applied, so either being assigned or passed
                         [func (let ((env-exprs (append-map (pe-helper env) (map Id-sym (Closure-env-params func)))))
                                 (list `(call $__allocate_func (i32.const ,(hash-ref func-table id)) ,(build-env env-exprs))))]
                         [else (error 'unknown "ERROR: Uknown Id ~v ~a ~a ~a" id globals env-params locals)]))]
    [(Float n) (list `(call $__allocate_float (f64.const ,n)))]
    [(Int n) (list `(call $__allocate_int (i64.const ,n)))]
    [other (error 'unsupported (~a expr))]))

(define (build-env [exprs : (Listof Sexp)]) : Sexp
  (foldl
   (lambda ([e : Sexp] [curr : Sexp]) `(call $__allocate_pair ,e ,curr))
   '(i32.const 0)
   exprs))

; TODO: I think we should have a primitive application form different from an indirect call form
(define (call-primitive [fn : Symbol] [p-args : (Listof Sexp)]) : (Listof Sexp)
  (list `(call ,(wat-name (hash-ref prims fn)) ,@p-args)))

; TODO: Actually handle exports, but low priority
(define (export? [fn : Symbol] [exports : (Listof Provide)]) : (Listof Sexp)
  (if (findf (lambda ([p : Provide]) (and (SimpleProvide? p) (equal? (SimpleProvide-id p) fn)))
             exports)
      (list `(export ,(~a '\" fn '\")))
      '()))

(define (wat-params [params : (Listof Symbol)]) : (Listof Sexp)
  (map (lambda ([p : Symbol]) `(param ,(wat-name p) i32)) params))

(define (wat-locals [locals : (Listof Symbol)]) : (Listof Sexp)
  ; TODO: I wonder if we actually need locals to be pointers too??
  (map (lambda ([l : Symbol]) `(local ,(wat-name l) i32)) locals))

(define (wat-results [fn : Symbol] [ret-types : (Listof Symbol)]) : (Listof Sexp)
  (if (equal? fn 'init) '() `((result ,@ret-types))))

(define (functype-name [s : Symbol]) : Symbol
  (wat-name (string->symbol (~a "ftype_" s))))

(define (wat-name [id : Symbol]) : Symbol
  (string->symbol (~a "$" id)))

(define (block-name [label : String]) : Symbol
  (wat-name (string->symbol (~a label (gi-bc)))))


(define block-counter 0)
(define (gi-bc) : Integer
  (let ((bc block-counter))
    (set! block-counter (add1 bc))
    bc))



#;(define (get-clo-env-params [src : Sexp] [size-env : Integer]) : (Listof Sexp)
  (map
   (lambda ([r : Real])
     (let ((nestedness (real->int r)))
       (gcep-helper src nestedness)))
   (range size-env)))

#;(define (gcep-helper [src : Sexp] [size : Integer]) : Sexp
  (if (equal? 0 size)
      ; Get the value of the final element in the env
      `(i32.load (i32.add (i32.const 1) (i32.load (i32.add (i32.const 5) ,@src))))
      `(i32.load (i32.add (i32.const 5) ,(gcep-helper src (sub1 size))))))


