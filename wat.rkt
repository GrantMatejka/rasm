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
     (\; All functions take a pointer to the parameter list and a pointer to their environment as parameters \;)
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
    `((table $tbl ,(length funcs) funcref)
      (export \"table\" (table $tbl))
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
              ,@(if (equal? fn 'init) '() (list '(param $param_arr i32) '(param $env_arr i32)))
              ,@(if (equal? fn 'init) '() (list `(result i32)))
              ,@(build-locals (map Id-sym (append locals params env-params)))
              ; Every function relies on these helper vars
              (local $__app_param_arr i32)
              (local $__clo_env_arr_helper i32)
              (local $__tmp_res i32)
              ,@(init-locals params)
              ,@(init-env-locals env-params)
              (local.set $__app_param_arr (i32.const 0))
              (local.set $__clo_env_arr_helper (i32.const 0))
              (local.set $__tmp_res (i32.const 0))
              ,@(process-func-body globals env-params params locals funcs body))]
      [other (error 'unsupported (~a clo))])))

(define (process-func-body [globals : (Listof Id)]
                           [env-params : (Listof Id)]
                           [params : (Listof Id)]
                           [locals : (Listof Id)]
                           [funcs : (Listof Closure)]
                           [body : (Listof Expr)]) : (Listof Sexp)
  (append-map (lambda ([e : Expr]) (process-expr globals env-params params locals funcs (make-hash) e)) body))

(define (process-expr [globals : (Listof Id)]
                      [env-params : (Listof Id)]
                      [params : (Listof Id)]
                      [locals : (Listof Id)]
                      [funcs : (Listof Closure)]
                      [env : Env]
                      [expr : Expr]) : (Listof Sexp)
  (define global? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) globals)))
  (define local? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) locals)))
  (define param? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) params)))
  (define env? (lambda ([id : Symbol]) (findf (lambda ([i : Id]) (equal? id (Id-sym i))) env-params)))
  (define (get-lam-binding [id : Symbol]) : Symbol
    (let ((binding (findf (lambda ([i : Id]) (equal? id (Id-sym i)))
                          (append locals params env-params globals)))
          (func (findf (lambda ([fn : Symbol]) (equal? id fn))
                       (map Closure-name funcs))))
      ; Var will overshadow func
      (if binding
          (if (anon? (Id-type binding))
              (Id-type binding)
              (Id-sym binding))
          (if func
              func
              (error 'id "Uknown id: ~a" id)))))
  
  (define pe-helper (lambda ([env : Env]) (lambda ([e : Expr]) (process-expr globals env-params params locals funcs env e))))
  (match expr
    [(Call fn args)
     ; TODO: See if we can combine these
     (if (equal? fn '__app)
         (let ((first-arg ((pe-helper env) (first args)))
               (rest-args (map (pe-helper env) (rest args))))
           `((\; Apply a first class function \;)
             ; TODO: This will collide if nested expressions are applied
             (local.set $__tmp_res ,@first-arg)
             ,@(build-arr rest-args '$__app_param_arr)
             (i32.load (i32.add (i32.const 5) (local.get $__tmp_res)))
             (call_indirect (type $__function_type) (i32.load (i32.add (i32.const 1) (local.get $__tmp_res))))))
         (list `(call ,(wat-name (hash-ref prims fn))
                      ,@(append-map (pe-helper env) args))))]
    [(IndirectCall fn args)
     (let ((p-fn ((pe-helper env) (hash-ref env fn (lambda () (get-lam-binding fn)))))
           (p-args (map (pe-helper env) args)))
       `((\; Preparing to Call ,fn \;)
         (\; Build function\'s parameter list \;)
         ,@(build-arr p-args '$__app_param_arr)
         (\; Retrieve Function's env \;)
         (i32.load (i32.add (i32.const 5) ,@p-fn))
         (\; Calling ,fn \;)
         (call_indirect (type $__function_type)
                        ,(if (hash-has-key? func-table fn)
                             `(i32.const ,(hash-ref func-table fn))
                             `(i32.load (i32.add (i32.const 1) ,@p-fn))))))]
    [(If test t f)
     (list `(if (result i32) (i64.eqz (i64.load (i32.add (i32.const 1) ,@((pe-helper env) test))))
                ; Since we are checking the conditional for eqz we need to reverse the conditions
                (then ,@((pe-helper env) f))
                (else ,@((pe-helper env) t))))]
    ; TODO: Actually handle ids and vals
    [(LetVals ids vals exprs)
     (append
      (filter-map
       (lambda ([l-id : (Listof Symbol)] [val : Expr])
         (cond
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
    [(? symbol? s) (letrec ((id&env (if (hash-has-key? env s)
                                        (let ((env-id (hash-ref env s))
                                              (new-env (make-hash (hash->list (hash-remove (make-immutable-hash (hash->list env)) s)))))
                                          (list env-id new-env))
                                        (list s env)))
                            (func (findf (lambda ([f : Closure]) (equal? (first id&env) (Closure-name f))) funcs)))
                     (define id (first id&env))
                     (define env (second id&env))
                     (cond
                       [(local? id) (list `(local.get ,(wat-name id)))]
                       ; Every param should be assigned to a local value by now, so we don't need to dereference the param arr all the time
                       [(param? id) (list `(local.get ,(wat-name id)))]
                       ; The env arr is just a flat array of pointers
                       [(env? id) (list `(local.get ,(wat-name id)))]
                       [(global? id) (list `(global.get ,(wat-name id)))]
                       ; If we ever encounter a function in this instance, it's not being applied, so either being assigned or passed
                       [func  (let ((env-exprs (map (pe-helper env) (map Id-sym (Closure-env-params func)))))
                                `((call $__allocate_func (i32.const ,(hash-ref func-table id))
                                        ,@(build-arr env-exprs '$__clo_env_arr_helper))))]
                       [else (error 'unknown "ERROR: Unknown Id ~v ~a ~a ~a ~a" id globals params env-params locals)]))]
    [(Float n) (list `(call $__allocate_float (f64.const ,n)))]
    [(Int n) (list `(call $__allocate_int (i64.const ,n)))]
    [other (error 'unsupported (~a expr))]))


(define (build-arr [exprs : (Listof (Listof Sexp))] [list-name : (U '$__app_param_arr '$__clo_env_arr_helper)]) : (Listof Sexp)
  `((local.set ,list-name (call $__alloc (i32.const ,(* 4 (length exprs)))))
    ,@(map
       (lambda ([src : Sexp] [r : Real])
         `(i32.store (i32.add (i32.const ,(* (real->int r) 4)) (local.get ,list-name)) ,@src))
       exprs
       (range (length exprs)))
    (local.get ,list-name)))

; TODO: Actually handle exports, but low priority
(define (export? [fn : Symbol] [exports : (Listof Provide)]) : (Listof Sexp)
  (if (findf (lambda ([p : Provide]) (and (SimpleProvide? p) (equal? (SimpleProvide-id p) fn)))
             exports)
      (list `(export ,(~a '\" fn '\")))
      '()))

(define (build-locals [locals : (Listof Symbol)]) : (Listof Sexp)
  (map (lambda ([l : Symbol]) `(local ,(wat-name l) i32)) locals))

(define (init-locals [locals : (Listof Id)]) : (Listof Sexp)
  (map
   (lambda ([id : Id] [r : Real])
     `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $param_arr)))))
   locals
   (range (length locals))))

(define (init-env-locals [locals : (Listof Id)]) : (Listof Sexp)
  (map
   (lambda ([id : Id] [r : Real])
     `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $env_arr)))))
   locals
   (range (length locals))))

(define (wat-name [id : Symbol]) : Symbol
  (string->symbol (~a "$" id)))



