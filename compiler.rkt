#lang racket

(require syntax/kerncase)
 
(define file-to-compile
  (command-line
   #:program "compiler"
   #:args (filename)
   filename))

(define in-file (open-input-file (string-append "test_programs/" file-to-compile ".rkt")))
(define out-file (open-output-file (string-append "output/" file-to-compile ".wat") #:exists 'replace))

(define module-object
  (parameterize ([current-namespace (make-base-namespace)])
    (expand (read-syntax "wasm" in-file))))


module-object
(display "\n\n\n")


; Only supported top level form is a module
(define (process-top-level syn)
  (kernel-syntax-case syn #f
    [(module name lang contents)
     `(module ,@(filter-map
                 (lambda (form)
                   (process-module form (gather-exports (syntax->list #'contents))))
                 (syntax->list #'contents)))]
    [other (error 'rasm "Unsupported: Only module compilation is supported. file: ~v" file-to-compile)]))

(define (process-module mod-form exports)
  (kernel-syntax-case mod-form #f
    ; TODO: hack to only work with one id max
    [(define-values ids expr) (process-definition (syntax-e (first (syntax->list #'ids))) #'expr exports)]
    ; We should already have exports
    [(#%provide spec) #f]
    [other #f]))

(define (process-definition id value exports)
  (kernel-syntax-case value #f
    [(#%plain-lambda args body) `(func
                                  ,(string->symbol (string-append "$" (symbol->string id)))
                                  ; TODO: proper way is ,(when (member id exports) `(export ,(symbol->string id)))
                                  ; but this will return #<void> when not exported so just hack all to export rn
                                  (export ,(symbol->string id))
                                  ; TODO: hack to only allow i32 params
                                  ,@(map (lambda (_) '(param i32)) (syntax-e #'args))
                                  ; TODO: hack to only allow i32 result
                                  (result i32)
                                  ,(process-expression #'body))]
    [other #f]))

; TODO: need an environment for Funcs and Vars and Mem
(define (process-expression expr)
  (kernel-syntax-case expr #f
    ; TODO: this is all basically a hack
    [(#%plain-app op l r) `(,(hash-ref built-in (syntax-e #'op))
                            (i32.const ,(second (syntax->datum #'l)))
                            (i32.const ,(second (syntax->datum #'r))))]
    [other (display expr)]))


(define (gather-exports module-contents)
  (filter-map (lambda (form) (kernel-syntax-case form #f
                               [(#%provide id) (syntax-e #'id)]
                               [other #f]))
              module-contents))


(define built-in (hash
                  '+ 'i32.add
                  '- 'i32.sub
                  '* 'i32.mul
                  '/ 'i32.div_u))



(write-to-file (process-top-level module-object) "output/output.wat" #:exists 'replace)




