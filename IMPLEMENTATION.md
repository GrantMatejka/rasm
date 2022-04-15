# Notes

I *believe* all indices in wasm are `u32` 
SOURCE: https://webassembly.github.io/spec/core/syntax/modules.html#syntax-tableidx

## Objects

We need an object for every complicated type in wasm. 
(I think we will temporarily even represent numbers as objects and hopefully optimize this out)

Every object will be identified by a pointer.

Type Id is the first `i8` of the pointer.
The next bytes after will be determined based on what type it is.

### Type Id's and Memory Data Layout

---

| Type Id | Name | Data Description |
| ---- | ---- | ---- |
| 0 | Integer | Read next 8 bytes as the value |
| 1 | Float | Read the next 8 bytes as the value |
| 2 | Function | Read the next i32 as the index to the function table and the i32 after as a pointer to the environment of the function |
| 3 | Pair | The first i32 is the pointer to the first value while the second i32 is the pointer to the second value |
| 4 | Char | Read next 8 bytes representing the Unicode character |
| 5 | String | The first i32 is a pointer to the chain of pairs of the chars and the second i32 is the length of the string |

---

### Booleans

Booleans are integers where anything non-zero is true while zero is false.

### Functions

We need a function Table:
-> Wasm Table of type 'anyfunc'

For any 'closure/func' we have an entry in the Table
Therefore we have a table of indexes to function labels.

| idx | func |
| -- | -- |
| 0 | $f |
| 1 | $g |
| 2 | $h |

So for our 'Closure' type we can use an i32 to represent the index to the table and an i32 to point to the env.

We must use tables to return function objects since we cannot return a label to a function, we can only return an index to the function as its location in the function table.

SOURCE: https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format#webassembly_tables

### Pairs

Represented in memory as an `i64` where the first `i32` is the pointer to the first value's memory location while the second `i32` is the pointer to the second value's memory location.

A list is just a bunch of nested pairs.

The terminating value will be `-1` to represent `'()` and `null`.
