#lang typed/racket

(require typed/rackunit)

; A program is a
;   list of imports
;   list of exports
;   globals
;   memory (basically a global)
;   functions

; WASM types i32, i64, f32, f64

; TODO: We will need more than one top level func env due to wasm types
(define built-in (hash
                  '+ 'i32.add
                  '- 'i32.sub
                  '* 'i32.mul
                  '/ 'i32.div_u))


(define (process-builtin [op : Symbol] [l : Sexp] [r : Sexp]) : Sexp
  `(,(hash-ref built-in op) ,l ,r))

; TODO: We can calculate return-type based on body
; TODO: All params hardcoded to i32
(define (wat-func [name : String] [params : (Listof Sexp)] [return-type : Sexp] [body : Sexp]) : Sexp
  `(func (export ,name) ,@(map (lambda (_) '(param i32)) params) (result i32) ,body))

; Get the names of each function to export in WAT
(define (gather-module-exports [module-body : (Listof Sexp)])
  (map (lambda (expr) (match expr [(list 'provide (? symbol? name)) name] [other 'null])) module-body))

(define (create-wat-module [expr : Sexp])
  ; expr can either be a racket module or just a single expression
  (match expr
    [(list 'module body ...)
     (define exports (gather-module-exports body))
     ; TODO: obviously a limitation, but not a necessary one
     (when (not (member 'main exports)) (error 'create-wat-module "Must export a main function ~v" expr))
     `(module
          ,@(filter-map
            (lambda ([sub-expr : Sexp]) (match sub-expr [(list 'provide _) #f] [other (compile sub-expr exports)])) body))]
    [other `(module ,(wat-func "main" '() 'null (compile expr '())))]))



(define (compile [expr : Sexp] [exports : (Listof Symbol)]) : Sexp
  (match expr
    ; TODO: this could be more forms than function definition
    [(list 'define (list (? symbol? name) params ...) body) (wat-func (symbol->string name) params 'null (compile body exports))]
    [(list (? symbol? op) l r)
     (cond
       [(hash-ref built-in op) (process-builtin op (compile l exports) (compile r exports))]
       [else (error 'compile "Unsupported Integer Operation ~v" expr)])]
    ; TODO: Need to process numbers better, can do Real for floats, but what about 32 v 64 bit?
    [(? integer? n) `(i32.const ,n)]
    [other (error 'compile "Unsupported Operation ~v" expr)]))


;; Insert what you want compiled here
(define compile-me '(/ 6 (+ 1 2)))


;; Actually create the wat file and module
(define out (open-output-file (string-append "output/output.wat") #:exists 'replace))
(write (create-wat-module compile-me) out)
(close-output-port out)




; ----- TESTS -----
(check-equal? (compile '(+ 1 2) '()) '(i32.add (i32.const 1) (i32.const 2)))
(check-equal? (compile '(- 2 1) '()) '(i32.sub (i32.const 2) (i32.const 1)))
(check-equal? (compile '(/ 2 1) '()) '(i32.div_u (i32.const 2) (i32.const 1)))
(check-equal? (compile '(* 2 1) '()) '(i32.mul (i32.const 2) (i32.const 1)))
(check-equal? (compile '(* 2 (+ 1 1)) '()) '(i32.mul (i32.const 2) (i32.add (i32.const 1) (i32.const 1))))
(check-equal? (create-wat-module '(module (provide main) (define (main a b) (+ 1 2))))
              '(module (func (export "main") (param i32) (param i32) (result i32) (i32.add (i32.const 1) (i32.const 2)))))
(check-exn (regexp (regexp-quote "Must export a main function"))
           (lambda () (create-wat-module '(module (provide f) (define (f a b) (+ 1 2))))))
