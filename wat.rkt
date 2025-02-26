#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(provide build-wat)

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
  (let ((func-names (filter (lambda ([s : Symbol]) (not (equal? s 'init))) (append (map Closure-name (Module-funcs mod))
                                                                                   (hash-values prims)))))
    `((table $tbl ,(length func-names) funcref)
      (export \"table\" (table $tbl))
      (elem (i32.const 0)
            ,@(map (lambda ([fn : Symbol] [idx : Real])
                     (hash-set! func-table fn (real->int idx))
                     (wat-name fn))
                   func-names
                   (range (length func-names)))))))

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
    (set! tmps '())
    ; At this point the only top level/non-global variable definitions we have are the functions themselves
    (match clo
      [(Closure fn params env-params locals body)
       (let ((body (process-func-body globals env-params params locals funcs body)))
         `(func ,(wat-name fn)
                ,@(export? fn exports)
                ,@(if (equal? fn 'init) '() (list '(param $param_arr i32) '(param $env_arr i32)))
                ,@(if (equal? fn 'init) '() (list `(result i32)))
                ,@(build-locals (append (map Id-sym (append locals params env-params))
                                        tmps))
                ; Every function relies on these helper vars
                (local $__app_param_arr i32)
                (local $__clo_env_arr_helper i32)
                (local $__tmp_res i32)
                ,@(if (equal? fn 'init) (init-globals (map Id-sym globals)) '())
                ,@(init-locals params)
                ,@(init-env-locals env-params)
                (local.set $__app_param_arr (i32.const 0))
                (local.set $__clo_env_arr_helper (i32.const 0))
                ,@(init-tmps tmps)
                ,@body))]
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
              (if (hash-has-key? prims id)
                  id
                  (error "Unknown Id ~v" id))))))
  
  (define pe-helper (lambda ([env : Env]) (lambda ([e : Expr]) (process-expr globals env-params params locals funcs env e))))
  (match expr
    [(Call fn args)
     (if (equal? fn '__app)
         (let ((first-arg ((pe-helper env) (first args)))
               (rest-args (map (pe-helper env) (rest args)))
               (tmp-var (g&i-tmp-res)))
           `((\; Apply a first class function \;)
             (call $__app ,@first-arg ,@(build-arr rest-args '__app_param_arr))))
         (list `(call ,(wat-name (hash-ref prims fn))
                      ,@(append-map (pe-helper env) args))))]
    [(IndirectCall fn args)
     (letrec ((top-func (findf (lambda ([clo : Closure]) (equal? (Closure-name clo) fn)) funcs))
              (p-env&fn (if (hash-has-key? prims fn)
                            ; Primitives have no environmental needs
                            (list `(i32.const 0)
                                  `(i32.const ,(hash-ref func-table (hash-ref prims fn))))
                            (if top-func
                                (list `(block (result i32) ,@(build-arr (map (pe-helper env)
                                                                             (map Id-sym (Closure-env-params top-func)))
                                                                        '__clo_env_arr_helper))
                                      `(i32.const ,(hash-ref func-table fn)))
                                (let ((fn-expr ((pe-helper env) (hash-ref env fn (lambda () (get-lam-binding fn))))))
                                  (list
                                   `(i32.load (i32.add (i32.const 5) ,@fn-expr))
                                   `(i32.load (i32.add (i32.const 1) ,@fn-expr)))))))
              (p-args (map (pe-helper env) args)))
       `(,@(build-arr p-args '__app_param_arr)
         ,(first p-env&fn)
         (call_indirect (type $__function_type)
                        ,(second p-env&fn))))]
    [(CaseLambda func-names)
     (let ((funcdefs (filter-map (lambda ([fn : Symbol])
                                   (findf (lambda ([clo : Closure]) (equal? (Closure-name clo) fn)) funcs))
                                 func-names)))
       (let ((p-len (wat-name (g&i-tmp-res)))
             (p-idx (wat-name (g&i-tmp-res))))
         `((block
            (loop
             (i32.eqz (i32.load (i32.add (local.get ,p-idx) (local.get $param_arr))))
             (br_if 1) ;; break
             ;; increment p-len
             (local.set ,p-len (i32.add (i32.const 1) (local.get ,p-len)))
             (local.set ,p-idx (i32.add (i32.const 4) (local.get ,p-idx)))
             (br 0)
             ))
           ,@(build-cl-checks (pe-helper env) p-len funcdefs))))]
    [(If test t f)
     (list `(if (result i32) (i64.eqz (i64.load (i32.add (i32.const 1) ,@((pe-helper env) test))))
                ; Since we are checking the conditional for eqz we need to reverse the conditions
                (then ,@((pe-helper env) f))
                (else ,@((pe-helper env) t))))]
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
    [(Set id expr) (let ((p-expr ((pe-helper env) expr))
                         (getter (if (findf (lambda ([i : Symbol]) (equal? i id)) (map Id-sym globals))
                                     `(global.get ,(wat-name id))
                                     `(local.get ,(wat-name id))))
                         (tmp (wat-name (g&i-tmp-res))))
                     `((local.set ,tmp ,@p-expr)
                       (i32.store8 ,getter (i32.load8_u (local.get ,tmp)))
                       (i64.store (i32.add (i32.const 1) ,getter) (i64.load (i32.add (i32.const 1) (local.get ,tmp))))))]
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
                       [(hash-has-key? prims s) `(,(wat-name (hash-ref prims s)))]
                       ; If we ever encounter a function in this instance, it's not being applied, so either being assigned or passed
                       [func  (let ((env-exprs (map (pe-helper env) (map Id-sym (Closure-env-params func)))))
                                `((\; Allocating func ,id \;)
                                  (call $__allocate_func (i32.const ,(hash-ref func-table id))
                                        ,@(build-arr env-exprs '__clo_env_arr_helper))))]
                       [else (error 'unknown "ERROR: Unknown Id ~v" id)]))]
    [(Float n) (list `(call $__allocate_float (f64.const ,n)))]
    [(Int n) (list `(call $__allocate_int (i64.const ,n)))]
    [other (error 'unsupported (~a expr))]))


(define (build-arr [exprs : (Listof (Listof Sexp))] [list-name : (U '__app_param_arr '__clo_env_arr_helper)]) : (Listof Sexp)
  (let ((unique-ln (unique list-name)))
    `((local.set ,(wat-name unique-ln) (call $__alloc (i32.const ,(* 4 (add1 (length exprs))))))
      ,@(map
         (lambda ([src : Sexp] [r : Real])
           `(i32.store (i32.add (i32.const ,(* 4 (real->int r))) (local.get ,(wat-name unique-ln))) ,@src))
         exprs
         (range (length exprs)))
      ;; zero terminate our array
      (i32.store (i32.add (i32.const ,(* 4 (length exprs))) (local.get ,(wat-name unique-ln))) (i32.const 0))
      (local.get ,(wat-name unique-ln)))))

; TODO: Actually handle exports, but low priority
(define (export? [fn : Symbol] [exports : (Listof Provide)]) : (Listof Sexp)
  (if (findf (lambda ([p : Provide]) (and (SimpleProvide? p) (equal? (SimpleProvide-id p) fn)))
             exports)
      (list `(export ,(~a '\" fn '\")))
      '()))

(define tmp 0)
(define tmps : (Listof Symbol) '())
(define (g&i-tmp-res)
  (let ((tmp-res (string->symbol (~a "__tmp_res" tmp))))
    (set! tmp (add1 tmp))
    (set! tmps (cons tmp-res tmps))
    tmp-res))

(define (unique [n : Symbol]) : Symbol
  (let ((tmp-name (string->symbol (~a n tmp))))
    (set! tmp (add1 tmp))
    (set! tmps (cons tmp-name tmps))
    tmp-name))


(define (build-locals [locals : (Listof Symbol)]) : (Listof Sexp)
  (map (lambda ([l : Symbol]) `(local ,(wat-name l) i32)) locals))

(define (init-globals [globals : (Listof Symbol)]) : (Listof Sexp)
  (map (lambda ([l : Symbol]) `(global.set ,(wat-name l) (call $__allocate_int (i64.const 0)))) globals))

(define (init-locals [locals : (Listof Id)]) : (Listof Sexp)
  (map
   (lambda ([id : Id] [r : Real])
     `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $param_arr)))))
   locals
   (range (length locals))))

(define (init-tmps [tmps : (Listof Symbol)]) : (Listof Sexp)
  (map
   (lambda ([id : Symbol])
     `(local.set ,(wat-name id) (i32.const 0)))
   (remove-duplicates tmps)))

(define (init-env-locals [locals : (Listof Id)]) : (Listof Sexp)
  (map
   (lambda ([id : Id] [r : Real])
     `(local.set ,(wat-name (Id-sym id)) (i32.load (i32.add (i32.const ,(* (real->int r) 4)) (local.get $env_arr)))))
   locals
   (range (length locals))))

(define (build-cl-checks [s->e : (-> Expr (Listof Sexp))] [len : Symbol] [funcs : (Listof Closure)]) : (Listof Sexp)
  (let ((func (first funcs)))
    (if (equal? 1 (length funcs))
        `((if (result i32) (i32.eq (local.get ,len) (i32.const ,(length (Closure-params func))))
              (then
               (local.get $param_arr)
               ,@(build-arr (map s->e (map Id-sym (Closure-env-params func))) '__clo_env_arr_helper)
               (call_indirect (type $__function_type) (i32.const ,(hash-ref func-table (Closure-name func)))))
              (else (i32.const -1))))
        `((if (result i32) (i32.eq (local.get ,len) (i32.const ,(length (Closure-params func))))
              (then
               (local.get $param_arr)
               ,@(build-arr (map s->e (map Id-sym (Closure-env-params func))) '__clo_env_arr_helper)
               (call_indirect (type $__function_type) (i32.const ,(hash-ref func-table (Closure-name func)))))
              (else ,@(build-cl-checks-helper s->e len (rest funcs))))))))

(define (build-cl-checks-helper [s->e : (-> Expr (Listof Sexp))] [len : Symbol] [funcs : (Listof Closure)]) : (Listof Sexp)
  (let ((func (first funcs)))
    (if (empty? (rest funcs))
        `((local.get $param_arr)
          ,@(build-arr (map s->e (map Id-sym (Closure-env-params func))) '__clo_env_arr_helper)
          (call_indirect (type $__function_type) (i32.const ,(hash-ref func-table (Closure-name func)))))
        `((if (result i32) (i32.eq (local.get ,len) (i32.const ,(length (Closure-params func))))
              (then
               (local.get $param_arr)
               ,@(build-arr (map s->e (map Id-sym (Closure-env-params func))) '__clo_env_arr_helper)
               (call_indirect (type $__function_type) (i32.const ,(hash-ref func-table (Closure-name func)))))
              (else ,@(build-cl-checks-helper s->e len (rest funcs))))))))

(define (wat-name [id : Symbol]) : Symbol
  (string->symbol (~a "$" id)))



