#lang typed/racket

; A program is a
;   list of imports
;   list of exports
;   globals
;   memory (basically a global)
;   functions

; WASM types i32, i64, f32, f64

; We will need more than one top level func env due to wasm types
(define built-in (hash
                  '+ 'i32.add
                  '- 'i32.sub
                  '* 'i32.mul
                  '/ 'i32.div_u))

(define (process-builtin [op : Symbol] [l : Sexp] [r : Sexp]) : Sexp
  `(,(hash-ref built-in op) ,l ,r))

(define (wat-func [expr : Sexp]) : Sexp
  `(func (export "main") (result i32) ,expr))

(define (create-wat-module [expr : Sexp])
  `(module ,(wat-func (compile expr))))

(define (compile [expr : Sexp]) : Sexp
  (match expr
    [(list (? symbol? op) l r)
     (cond
       [(hash-ref built-in op) (process-builtin op (compile l) (compile r))]
       [else (error 'compile "Unsupported Integer Operation ~v" expr)])]
    [(? integer? n) `(i32.const ,n)]
    [other (error 'compile "Unsupported Operation ~v" expr)]))


(compile '(+ 1 2))
(compile '(- 2 1))
(compile '(* 1 2))
(compile '(/ 1 2))
(compile '(/ 1 (+ 1 2)))


;; Actually create the wat file and module
(define out (open-output-file (string-append "output/output.wat") #:exists 'replace))
(write (create-wat-module '(/ 6 (+ 1 2))) out)
(close-output-port out)