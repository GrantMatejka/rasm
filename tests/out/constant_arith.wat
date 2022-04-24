(module (; This gives us 256 pages of memory where each page is 64KiB ;) (memory $mem 256)
  (export "memory" (memory $mem))
  (global $__mem_head (mut i32) (i32.const 1))
  (; ------------------------------ ;)
  (; ------------------------------ ;)
  (; ------------start------------- ;)
  (; ------------------------------ ;)
  (; ------------------------------ ;)
  (table 12 funcref)
  (elem (i32.const 0) $add1 $add2 $add3 $sub1 $sub2 $sub3 $mul1 $mul2 $mul3 $div1 $div2 $div3)
  (type $ftype_add1 (func (result i32)))
  (type $ftype_add2 (func (result i32)))
  (type $ftype_add3 (func (result i32)))
  (type $ftype_sub1 (func (result i32)))
  (type $ftype_sub2 (func (result i32)))
  (type $ftype_sub3 (func (result i32)))
  (type $ftype_mul1 (func (result i32)))
  (type $ftype_mul2 (func (result i32)))
  (type $ftype_mul3 (func (result i32)))
  (type $ftype_div1 (func (result i32)))
  (type $ftype_div2 (func (result i32)))
  (type $ftype_div3 (func (result i32)))
  (func $init (local $__env_helper i32))
  (func
   $add1
   (export "add1")
   (result i32)
   (local $__env_helper i32)
   (call $__add (call $__allocate_int (i64.const 41234)) (call $__allocate_int (i64.const 4508))))
  (func
   $add2
   (export "add2")
   (result i32)
   (local $__env_helper i32)
   (call $__add (call $__allocate_float (f64.const 4.5)) (call $__allocate_float (f64.const 10.2))))
  (func
   $add3
   (export "add3")
   (result i32)
   (local $__env_helper i32)
   (call $__add (call $__allocate_float (f64.const 4.5)) (call $__allocate_int (i64.const 4))))
  (func
   $sub1
   (export "sub1")
   (result i32)
   (local $__env_helper i32)
   (call $__sub (call $__allocate_int (i64.const 123435)) (call $__allocate_int (i64.const 35))))
  (func
   $sub2
   (export "sub2")
   (result i32)
   (local $__env_helper i32)
   (call $__sub (call $__allocate_float (f64.const 2.5)) (call $__allocate_float (f64.const 1.2))))
  (func
   $sub3
   (export "sub3")
   (result i32)
   (local $__env_helper i32)
   (call $__sub (call $__allocate_float (f64.const 2.5)) (call $__allocate_int (i64.const 1))))
  (func
   $mul1
   (export "mul1")
   (result i32)
   (local $__env_helper i32)
   (call $__mul (call $__allocate_int (i64.const 100)) (call $__allocate_int (i64.const 80))))
  (func
   $mul2
   (export "mul2")
   (result i32)
   (local $__env_helper i32)
   (call $__mul (call $__allocate_float (f64.const 2.5)) (call $__allocate_float (f64.const 10.5))))
  (func
   $mul3
   (export "mul3")
   (result i32)
   (local $__env_helper i32)
   (call $__mul (call $__allocate_float (f64.const 2.5)) (call $__allocate_int (i64.const 10))))
  (func
   $div1
   (export "div1")
   (result i32)
   (local $__env_helper i32)
   (call $__div (call $__allocate_int (i64.const 4500)) (call $__allocate_int (i64.const 2))))
  (func
   $div2
   (export "div2")
   (result i32)
   (local $__env_helper i32)
   (call
    $__div
    (call $__allocate_float (f64.const 450.5))
    (call $__allocate_float (f64.const 9.5))))
  (func
   $div3
   (export "div3")
   (result i32)
   (local $__env_helper i32)
   (call $__div (call $__allocate_float (f64.const 2.5)) (call $__allocate_int (i64.const 2))))
  (start $init)
  (; ------------------------------ ;)
  (; ------------------------------ ;)
  (; -------------end-------------- ;)
  (; ------------------------------ ;)
  (; ------------------------------ ;)
  (; Takes in number of bytes to allocate and returns the ptr to the start of the object ;)
  (; The number of bytes should not include the type id for the object ;)
  (func
   $__alloc
   (param $size i32)
   (result i32)
   (local $old_head i32)
   (local.set $old_head (global.get $__mem_head))
   (; Add 1 byte for the type id ;)
   (global.set
    $__mem_head
    (i32.add (i32.const 1) (i32.add (global.get $__mem_head) (local.get $size))))
   (return (local.get $old_head)))
  (func
   $__allocate_int
   (param $val i64)
   (result i32)
   (local $ptr i32)
   (local.set $ptr (call $__alloc (i32.const 5)))
   (i32.store8 (local.get $ptr) (i32.const 0))
   (i64.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val))
   (local.get $ptr))
  (func
   $__allocate_float
   (param $val f64)
   (result i32)
   (local $ptr i32)
   (local.set $ptr (call $__alloc (i32.const 5)))
   (i32.store8 (local.get $ptr) (i32.const 1))
   (f64.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val))
   (local.get $ptr))
  (func
   $__allocate_pair
   (param $val1_ptr i32)
   (param $val2_ptr i32)
   (result i32)
   (local $ptr i32)
   (local.set $ptr (call $__alloc (i32.const 9)))
   (i32.store8 (local.get $ptr) (i32.const 2))
   (i32.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $val1_ptr))
   (i32.store (i32.add (i32.const 5) (local.get $ptr)) (local.get $val2_ptr))
   (local.get $ptr))
  (func
   $__allocate_func
   (param $ft_idx i32)
   (param $env_ptr i32)
   (result i32)
   (local $ptr i32)
   (local.set $ptr (call $__alloc (i32.const 9)))
   (i32.store8 (local.get $ptr) (i32.const 3))
   (i32.store (i32.add (i32.const 1) (local.get $ptr)) (local.get $ft_idx))
   (i32.store (i32.add (i32.const 5) (local.get $ptr)) (local.get $env_ptr))
   (local.get $ptr))
  (func
   $__add
   (param $v1 i32)
   (param $v2 i32)
   (result i32)
   (if (result i32)
     (i32.eqz (i32.load8_u (local.get $v1)))
     (then
      (; v1 === int ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_int
          (i64.add
           (i64.load (i32.add (i32.const 1) (local.get $v1)))
           (i64.load (i32.add (i32.const 1) (local.get $v2))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.add
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v1))))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))
     (else
      (; v1 === float ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_float
          (f64.add
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.add
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))))
  (func
   $__sub
   (param $v1 i32)
   (param $v2 i32)
   (result i32)
   (if (result i32)
     (i32.eqz (i32.load8_u (local.get $v1)))
     (then
      (; v1 === int ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_int
          (i64.sub
           (i64.load (i32.add (i32.const 1) (local.get $v1)))
           (i64.load (i32.add (i32.const 1) (local.get $v2))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.sub
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v1))))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))
     (else
      (; v1 === float ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_float
          (f64.sub
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.sub
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))))
  (func
   $__mul
   (param $v1 i32)
   (param $v2 i32)
   (result i32)
   (if (result i32)
     (i32.eqz (i32.load8_u (local.get $v1)))
     (then
      (; v1 === int ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_int
          (i64.mul
           (i64.load (i32.add (i32.const 1) (local.get $v1)))
           (i64.load (i32.add (i32.const 1) (local.get $v2))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.mul
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v1))))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))
     (else
      (; v1 === float ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_float
          (f64.mul
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.mul
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))))
  (func
   $__div
   (param $v1 i32)
   (param $v2 i32)
   (result i32)
   (if (result i32)
     (i32.eqz (i32.load8_u (local.get $v1)))
     (then
      (; v1 === int ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_int
          (i64.div_s
           (i64.load (i32.add (i32.const 1) (local.get $v1)))
           (i64.load (i32.add (i32.const 1) (local.get $v2))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.div
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v1))))
           (f64.load (i32.add (i32.const 1) (local.get $v2))))))))
     (else
      (; v1 === float ;)
      (if (result i32)
        (i32.eqz (i32.load8_u (local.get $v1)))
        (then
         (; v2 === int ;)
         (call
          $__allocate_float
          (f64.div
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.reinterpret/i64 (i64.load (i32.add (i32.const 1) (local.get $v2)))))))
        (else
         (; v2 === float ;)
         (call
          $__allocate_float
          (f64.div
           (f64.load (i32.add (i32.const 1) (local.get $v1)))
           (f64.load (i32.add (i32.const 1) (local.get $v2)))))))))))
