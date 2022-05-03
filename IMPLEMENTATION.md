## MISC

Every function will take in pointers to their parameters and returns an i32 which serves as an pointer to the returned object in memory.

All indices in wasm are `u32` [SOURCE](https://webassembly.github.io/spec/core/syntax/modules.html#syntax-tableidx).

## Memory

In WebAssembly memory is simply an array of bytes.

We claim 256 pages where each page is 64KiB.

We allocate the memory on the WebAssembly side and export it, rather than relying on the JS host to allocate and import it to us.
- The memory is still mutable from both sides.

## Objects

We need an object for every complicated type in wasm. 

Every object will be identified by a pointer.

Type Id is the first `i8` of the pointer.
The next bytes after will be determined based on what type it is.

A list is a sequence of pairs, with 0 as the terminating pointer.

Chars are simply i64's. And strings are lists of chars.

### Type Id's and Memory Data Layout

---

| Type Id |   Name   | Data Description |
| ------  |   ----   | ---- |
|   0     | Integer  | Read next 8 bytes as the value |
|   1     | Float    | Read the next 8 bytes as the value |
|   2     | Pair     | The first i32 is the pointer to the first value while the second i32 is the pointer to the second value |
|   3     | Function | Read the next i32 as the index to the function table and the i32 after as a pointer to the environment of the function, the environment of the function is a flat array of pointers, since environment needs are known at compile time |

---

### Booleans

Booleans are integers where anything non-zero is true while zero is false.

### Functions

We need a function Table:
-> Wasm Table of type 'anyfunc'

For any 'closure/func' we have an entry in the Table
Therefore we have a table of indexes to function labels.

| idx | func |
| --  | --   |
| 0   | $f   |
| 1   | $g   |
| 2   | $h   |

So for our 'Closure' type we can use an i32 to represent the index to the table and an i32 to point to the env.

We must use tables to return function objects since we cannot return a label to a function, we can only return an index to the function as its location in the function table [SOURCE](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format#webassembly_tables).

#### Example

```scheme
(module add racket
  (provide add)

  ; Returns an `adder` function
  ;  where `x` should be bound
  ;  in the `adder` function's env
  (define (make-adder x)
    (lambda (y) (+ x y)))

  ; `a` should be a function object
  ;  with an env of `x` => 19
  (define a (make-adder 19))


  ; Dereferences function object a
  ; -> call_indirect with idx of a
  ; -> env populates x with 19
  ; Should return 22
  (define (add) (a 3)))
  ```


### Pairs

Represented in memory as an `i64` where the first `i32` is the pointer to the first value's memory location while the second `i32` is the pointer to the second value's memory location.

A list is just a bunch of nested pairs.

The terminating value will be `-1` to represent `'()` and `null`.
