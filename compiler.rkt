#lang racket

(require "expand.rkt")
(require "passes.rkt")
(require "wat.rkt")
(require racket/pretty
         json)

;; TODO: Things of interest
; - handling lambdas, basically make every lambda a top level func??? and use table to refer to them all
; -- Maybe take the top level at first and then try to optimize later (with loops)
; -- Case lambda maybe we can do this with a unique naming scheme
; - data types, just hardcode everything to f64 for now bc that's a safe bet
; - Provide/exports, we need to actually parse the spec
; - Convert Closures -> basically what CS does, need to use wasm tables

; Do we need to have a unique identifier for every var?
;   Don't know since wasm has func scoped vars, but I tink easy enough to just do to be safe
; RacketScript uses syntax-parse rather than kernel-syntax-case????

; Things of interest:
;  How are we handling types? var & return?
;  Make every lambda top level? Where we pass in all vars we're interested in from env?
;  Mmultiple return values?


; TODO: Get list libraries compiling
; What primitive functions do we really need to implement???
#;(let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require ''#%kernel)
      (namespace-require ''#%unsafe)
      (namespace-require ''#%flfxnum)
      (namespace-require ''#%extfl)
      (namespace-require ''#%futures)
      (namespace-require ''#%foreign)
      (namespace-mapped-symbols)))

; Datatypes of interest: string, pair, list

; HELP:
; How do we get a fully complete racket program?? Can we pull in all requirements for a fully fully expanded???
(define in #f)
(define out #f)

(command-line
 #:once-any
 [("--output") file "write output to output <file>"
               (set! out (open-output-file file #:exists 'replace))]
 [("--stdout") "write output to standard out"
               (set! out (current-output-port))]
 #:args ([source #f])
 (cond [(and in source)
        (raise-user-error "can't supply --stdin with a source file")]
       [source
        ; TODO: Eventually use this
        #;(when (not (output-port? out))
            (set! out (open-output-file (string-append source ".wat")
                                        #:exists 'replace)))
        (set! in source)]))

(define input (open-input-file in))
(define in-path (normalize-path in))

; TODO: delete this next line when we allow custom out files
(set! out (current-output-port))
(unless (output-port? out)
  (raise-user-error "no output specified"))

(unless (input-port? input)
  (raise-user-error "no input specified"))

(define basename (first (string-split (last (string-split in "/")) ".")))

(define EXPANDED (expand-file in))
(display-to-file EXPANDED
                 (string-append "dev/expanded/" basename "_exp.rkt")
                 #:exists 'replace)

(define AST (build-ast EXPANDED))
(display-to-file AST
                 (string-append "dev/ast/" basename "_ast.rkt")
                 #:exists 'replace)

(define PASSED-AST (full-pass AST))
(display-to-file PASSED-AST
                 (string-append "dev/ast/" basename "_passed_ast.rkt")
                 #:exists 'replace)

(define WAT (build-wat PASSED-AST))
(display-to-file WAT
                 (string-append "out/" basename ".wat")
                 #:exists 'replace)

(newline out)
(flush-output out)
