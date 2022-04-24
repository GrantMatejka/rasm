#lang typed/racket

(provide (all-defined-out))


; Whether we have an anonymous lambda or not
(define (anon? [fn : Symbol]) : Boolean
  (string-contains? (~a fn) "__lambda"))

(define (real->int [r : Real]) : Integer
  (cast (round r) Integer))

(define prims (hash
               '+ '__add
               '- '__sub
               '* '__mul
               '/ '__div
               '< 'f64.lt
               'equal? 'f64.eq))

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

(define wat-stdlib `((\; "Takes in number of bytes to allocate and returns the ptr to the start of the object" \;)
                     (\; "The number of bytes should not include the type id for the object" \;)
                     (func $__alloc (param $size i32) (result i32) (local $old_head i32)
                           (local.set $old_head (global.get $__mem_head))
                           (\; "Add 1 byte for the type id" \;)
                           (global.set $__mem_head (i32.add (i32.const 1)
                                                            (i32.add (global.get $__mem_head)
                                                                     (local.get $size))))
                           (return (local.get $old_head)))
                     (func $__allocate_int (param $val i64) (result i32) (local $ptr i32)
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
                     ,(simple-arith '$__add 'i64.add 'f64.add)
                     ,(simple-arith '$__sub 'i64.sub 'f64.sub)
                     ,(simple-arith '$__mul 'i64.mul 'f64.mul)
                     ,(simple-arith '$__div 'i64.div_s 'f64.div)))

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
