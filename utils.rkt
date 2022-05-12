#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

; Whether we have an anonymous lambda or not
(define (anon? [fn : Symbol]) : Boolean
  (string-contains? (~a fn) "__lambda"))

; Rounds a real number to an int
(define (real->int [r : Real]) : Integer
  (cast (round r) Integer))

(define (Provide? [a : Any]) : Boolean
  (or (SimpleProvide? a) (RenamedProvide? a) (AllDefined? a) (PrefixAllDefined? a)))

; Just like findf but returns user defined value if not found
(: my-findf (All (a) (-> (-> a Boolean) (Listof a) (U a False) (U a False))))
(define (my-findf lam l [not-found #f])
  (let ((found? (findf lam l)))
    (if found? found? not-found)))

(define prims (hash
               '+ '__add
               '- '__sub
               '* '__mul
               '/ '__div
               '< '__lt
               '> '__gt
               '<= '__le
               '>= '__ge
               'equal? '__eq
               '__app '__app))



(define (simple-arith [name : Symbol] [i64op : Symbol] [f64op : Symbol]) : Sexp
  `(func ,name (param $v1 i32) (param $v2 i32) (result i32)
         (if (result i32) (i32.eqz (i32.load8_u (local.get $v1)))
             (then (\; v1 === int \;)
                   (if (result i32) (i32.eqz (i32.load8_u (local.get $v2)))
                       (then (\; v2 === int \;)
                             (call $__allocate_int
                                   (,i64op (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                           (i64.load (i32.add (i32.const 1) (local.get $v2))))))
                       (else (\; v2 === float \;)
                             (call $__allocate_float
                                   (,f64op (f64.convert_i64_s (i64.load (i32.add (i32.const 1) (local.get $v1))))
                                           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))
             (else (\; v1 === float \;)
                   (if (result i32) (i32.eqz (i32.load8_u (local.get $v2)))
                       (then (\; v2 === int \;)
                             (call $__allocate_float
                                   (,f64op (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                           (f64.convert_i64_s (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
                       (else (\; v2 === float \;)
                             (call $__allocate_float
                                   (,f64op (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                           (f64.load (i32.add (i32.const 1) (local.get $v2)))))))))))

(define (simple-comp [name : Symbol] [i64op : Symbol] [f64op : Symbol]) : Sexp
  `(func ,name (param $v1 i32) (param $v2 i32) (result i32)
         (if (result i32) (i32.eqz (i32.load8_u (local.get $v1)))
             (then (\; v1 === int \;)
                   (if (result i32) (i32.eqz (i32.load8_u (local.get $v2)))
                       (then (\; v2 === int \;)
                             (call $__allocate_int
                                   (i64.extend_i32_s (,i64op (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                                         (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
                       (else (\; v2 === float \;)
                             (call $__allocate_int
                                   (i64.extend_i32_s (,f64op (f64.convert_i64_s (i64.load (i32.add (i32.const 1) (local.get $v1))))
                                                         (f64.load (i32.add (i32.const 1) (local.get $v2)))))))))
             (else (\; v1 === float \;)
                   (if (result i32) (i32.eqz (i32.load8_u (local.get $v2)))
                       (then (\; v2 === int \;)
                             (call $__allocate_int
                                   (i64.extend_i32_s (,f64op (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                         (f64.convert_i64_s (i64.load (i32.add (i32.const 1) (local.get $v2))))))))
                       (else (\; v2 === float \;)
                             (call $__allocate_int
                                   (i64.extend_i32_s (,f64op (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                         (f64.load (i32.add (i32.const 1) (local.get $v2))))))))))))

(define wat-stdlib `((\; "Takes in number of bytes to allocate and returns the ptr to the start of the object" \;)
                     (\; "The number of bytes should not include the type id for the object" \;)
                     (func $__alloc (export \"__alloc\") (param $size i32) (result i32) (local $old_head i32)
                           (local.set $old_head (global.get $__mem_head))
                           (\; "Add 1 byte for the type id" \;)
                           (global.set $__mem_head (i32.add (i32.const 1)
                                                            (i32.add (global.get $__mem_head)
                                                                     (local.get $size))))
                           (return (local.get $old_head)))
                     (func $__store_i32 (export \"__store_i32\") (param $loc i32) (param $ptr i32)
                           (i32.store (local.get $loc) (local.get $ptr)))
                     (\; Types: 0-Integer  1-Float  2-Pair  3-Function \;)
                     (func $__allocate_int (export \"__allocate_int\") (param $val i64) (result i32) (local $ptr i32)
                           (local.set $ptr (call $__alloc (i32.const 8)))
                           (i32.store8 (local.get $ptr) (i32.const 0))
                           (i64.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val))
                           (local.get $ptr))
                     (\; Need to make sure we always run with --experimental-wasm-bigint as shown by v8 team \;)
                     (func $__allocate_float (export \"__allocate_float\") (param $val f64) (result i32) (local $ptr i32)
                           (local.set $ptr (call $__alloc (i32.const 8)))
                           (i32.store8 (local.get $ptr) (i32.const 1))
                           (f64.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val))
                           (local.get $ptr))
                     (func $__allocate_pair (param $val1_ptr i32) (param $val2_ptr i32) (result i32) (local $ptr i32)
                           (local.set $ptr (call $__alloc (i32.const 8)))
                           (i32.store8 (local.get $ptr) (i32.const 2))
                           (i32.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val1_ptr))
                           (i32.store (i32.add (i32.const 5) (local.get $ptr)) (local.get $val2_ptr))
                           (local.get $ptr))
                     (func $__allocate_func (param $ft_idx i32) (param $env_ptr i32) (result i32) (local $ptr i32)
                           (local.set $ptr (call $__alloc (i32.const 8)))
                           (i32.store8 (local.get $ptr) (i32.const 3))
                           (i32.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $ft_idx))
                           (i32.store (i32.add (i32.const 5) (local.get $ptr)) (local.get $env_ptr))
                           (local.get $ptr))
                     (func $__app (export \"__app\") (param $func_ptr i32) (param $param_list i32) (result i32)
                           (if (result i32) (i32.eq (i32.const 3) (i32.load8_u (local.get $func_ptr)))
                               (then (local.get $param_list)
                                     (\; Pointer to function env \;)
                                     (i32.load (i32.add (i32.const 5) (local.get $func_ptr)))
                                     (call_indirect (type $__function_type) (i32.load (i32.add (i32.const 1) (local.get $func_ptr)))))
                               (else (i32.const -1))))
                     (func $__list_ref (param $root_ptr i32) (param $idx i32) (result i32)
                           (local $curr_ptr i32)
                           (local.set $curr_ptr (local.get $root_ptr))
                           (block $break
                                  (loop $loop
                                        (i32.eqz (local.get $idx))
                                        (br_if $break)
                                        (local.set $idx (i32.sub (local.get $idx) (i32.const 1)))
                                        (local.set $curr_ptr
                                                   (i32.load (i32.add (i32.const 5) (local.get $curr_ptr))))))
                           (i32.load (i32.add (i32.const 1) (local.get $curr_ptr))))
                     ,(simple-arith '$__add 'i64.add 'f64.add)
                     ,(simple-arith '$__sub 'i64.sub 'f64.sub)
                     ,(simple-arith '$__mul 'i64.mul 'f64.mul)
                     ,(simple-arith '$__div 'i64.div_s 'f64.div)
                     ,(simple-comp '$__gt 'i64.gt_s 'f64.gt)
                     ,(simple-comp '$__lt 'i64.lt_s 'f64.lt)
                     ,(simple-comp '$__ge 'i64.ge_s 'f64.ge)
                     ,(simple-comp '$__le 'i64.le_s 'f64.le)
                     ,(simple-comp '$__eq 'i64.eq 'f64.eq)))

#|
(func $__add (param $v1 i32) (param $v2 i32) (result i32)
                           (if (result i32) (i32.gt_u (i32.add
                                          (i32.load8_u (local.get $v1))
                                          (i32.load8_u (local.get $v2)))
                                         (i32.const 1))
                               (then (call $__allocate_float
                                           (f64.add (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (f64.load (i32.add (i32.const 1) (local.get $v2))))))
                               (else (call $__allocate_int
                                           (i64.add (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (i64.load (i32.add (i32.const 1) (local.get $v2))))))))
                     (func $__sub (param $v1 i32) (param $v2 i32) (result i32)
                           (if (result i32) (i32.gt_u (i32.add
                                          (i32.load8_u (local.get $v1))
                                          (i32.load8_u (local.get $v2)))
                                         (i32.const 1))
                               (then (call $__allocate_float
                                           (f64.sub (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (f64.load (i32.add (i32.const 1) (local.get $v2))))))
                               (else (call $__allocate_int
                                           (i64.sub (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (i64.load (i32.add (i32.const 1) (local.get $v2))))))))
                     (func $__mul (param $v1 i32) (param $v2 i32) (result i32)
                           (if (result i32) (i32.gt_u (i32.add
                                          (i32.load8_u (local.get $v1))
                                          (i32.load8_u (local.get $v2)))
                                         (i32.const 1))
                               (then (call $__allocate_float
                                           (f64.mul (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (f64.load (i32.add (i32.const 1) (local.get $v2))))))
                               (else (call $__allocate_int
                                           (i64.mul (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (i64.load (i32.add (i32.const 1) (local.get $v2))))))))
                     (func $__div (param $v1 i32) (param $v2 i32) (result i32)
                           (if (result i32) (i32.gt_u (i32.add
                                          (i32.load8_u (local.get $v1))
                                          (i32.load8_u (local.get $v2)))
                                         (i32.const 1))
                               (then (call $__allocate_float
                                           (f64.div (f64.load (i32.add (i32.const 1) (local.get $v1)))
                                                    (f64.load (i32.add (i32.const 1) (local.get $v2))))))
                               (else (call $__allocate_int
                                           (i64.div_s (i64.load (i32.add (i32.const 1) (local.get $v1)))
                                                      (i64.load (i32.add (i32.const 1) (local.get $v2))))))))
|#
