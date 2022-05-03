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
              ; Initialize param and env-param locals
              ,@(init-locals (append params env-params))
              #;,@(map
                 (lambda ([id : Id] [r : Real])
                   `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $param_arr)))))
                 (append params env-params)
                 (range (length (append params env-params))))
              (local.set $__app_param_arr (i32.const 0))
              (local.set $__clo_env_arr_helper (i32.const 0))
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
  (define pe-helper (lambda ([env : Env]) (lambda ([e : Expr]) (process-expr globals env-params params locals funcs env e))))
  (match expr
    [(Call fn args)
     (if (equal? fn '__app)
         (list `(\; Apply a first class function \;)
               `(if (result i32) (i32.eq (i32.const 3) (i32.load8_u ,@((pe-helper env) (first args))))
                    (then ,@(build-arr (append-map (pe-helper env) (rest args)) '$__app_param_arr)
                          (\; Pointer to function env \;)
                          (i32.load (i32.add (i32.const 5) ,@((pe-helper env) (first args))))
                          (call_indirect (type $__function_type) (i32.load (i32.add (i32.const 1) ,@((pe-helper env) (first args))))))
                    (else (i32.const -1)))

               #;`(call ,(wat-name (hash-ref prims fn))
                        ,@((pe-helper env) (first args))
                        ,@(setup-param-list (rest args) (pe-helper env))))
         (list `(call ,(wat-name (hash-ref prims fn))
                      ,@(append-map (pe-helper env) args))))]
    [(IndirectCall fn args)
     (let ((p-fn (hash-ref env fn (lambda () fn))))
           ; Otherwise we need to call the function indirectly
           (letrec ((funcname (let ((var? (findf (lambda ([id : Id]) (equal? (Id-sym id) p-fn)) (append globals env-params locals)))
                                 (static? (findf (lambda ([clo : Closure]) (equal? (Closure-name clo) p-fn)) funcs)))
                             (if var? (Id-type var?)
                                 (if static? (Closure-name static?) (error 'application "Unknown Function: ~a" fn)))))
                 (pp-fn ((pe-helper env) p-fn)))
             #;`((\; Set up parameters for indirect call \;)
               (local.set $__app_local_arr_ptr (call $__alloc (i32.const ,(* 4 (length args)))))
               ,@(map
                  (lambda ([src : Sexp] [r : Real])
                    `(i32.store (i32.add (i32.const ,(* (real->int r) 4)) (local.get $__app_local_arr_ptr)) ,src))
                  (append-map (pe-helper env) args)
                  (range (length args)))
               (\; Pointer to arguments for function call \;)
               (local.get $__app_local_arr_ptr))
             `(,@(build-arr (append-map (pe-helper env) args) '$__app_param_arr)
               (\; Pointer to env of the function we\'re calling \;)
               (i32.load (i32.add (i32.const 5) ,@pp-fn))
               (call_indirect (type $__function_type)
                              ,(if (hash-has-key? func-table fn)
                                   `(i32.const ,(hash-ref func-table fn))
                                   `(i32.load (i32.add (i32.const 1) ,@pp-fn)))))))]
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
         `(local.set ,(wat-name (first l-id)) ,@((pe-helper env) val))
         #;(cond
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
    [(? symbol? id) (letrec ((p-id (if (hash-has-key? env id)
                                       (let ((val (hash-ref env id)))
                                         (hash-remove! env id)
                                         val)
                                       id))
                             (func (findf (lambda ([f : Closure]) (equal? p-id (Closure-name f))) funcs)))
                      
                      (cond
                        [(local? p-id) (list `(local.get ,(wat-name p-id)))]
                        ; TODO: The param list is just a flat array of pointers
                        [(param? p-id) (list `(i32.load
                                               (i32.add (local.get $param_arr)
                                                        (i32.mul (i32.const 4)
                                                                 (i32.const ,(index-of params p-id (lambda ([env-id : Id] [id : Symbol]) (equal? (Id-sym env-id) p-id))))))))]
                        [(env? p-id) (list `(i32.load
                                               (i32.add (local.get $env_arr)
                                                        (i32.mul (i32.const 4)
                                                                 (i32.const ,(index-of env-params p-id (lambda ([env-id : Id] [id : Symbol]) (equal? (Id-sym env-id) p-id)))))))
                                           #;`(call $__list_ref
                                                  (local.get $env_list)
                                                  (i32.const ,(index-of env-params p-id (lambda ([env-id : Id] [id : Symbol]) (equal? (Id-sym env-id) p-id))))))]
                        [(global? p-id) (list `(global.get ,(wat-name p-id)))]
                        ; If we ever encounter a function in this instance, it's not being applied, so either being assigned or passed
                        [func (let ((env-exprs (append-map (pe-helper env) (map Id-sym (Closure-env-params func)))))
                                `((call $__allocate_func (i32.const ,(hash-ref func-table p-id))
                                        ,@(build-arr env-exprs '$__clo_env_arr_helper))))]
                        [else (error 'unknown "ERROR: Uknown Id ~v ~a ~a ~a" p-id globals env-params locals)]))]
    [(Float n) (list `(call $__allocate_float (f64.const ,n)))]
    [(Int n) (list `(call $__allocate_int (i64.const ,n)))]
    [other (error 'unsupported (~a expr))]))

(define (build-arr [exprs : (Listof Sexp)] [list-name : (U '$__app_param_arr '$__clo_env_arr_helper)]) : (Listof Sexp)
  `((local.set ,list-name (call $__alloc (i32.const ,(* 4 (length exprs)))))
    ,@(map
       (lambda ([src : Sexp] [r : Real])
         `(i32.store (i32.add (i32.const ,(* (real->int r) 4)) (local.get ,list-name)) ,src))
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

(define (wat-name [id : Symbol]) : Symbol
  (string->symbol (~a "$" id)))





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


