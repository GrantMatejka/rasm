#lang racket

(require racket/pretty
         "expand.rkt"
         "passes.rkt"
         "wat.rkt")

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

(pretty-print-columns 100)

(define in #f)
(define out #f)
(define dev #f)

(command-line
 #:once-any
 [("--output") file "write output to output <file>"
               (set! out (open-output-file file #:exists 'replace))]
 [("--stdout") "write output to standard out"
               (set! out (current-output-port))]
 [("--dev") "write intermediate forms to dev files. Expects \"dev/expanded\" and \"dev/ast\" to exist"
            (set! dev #t)]
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

(when dev (when (not (directory-exists? "dev")) (make-directory "dev")))
(when dev (when (not (directory-exists? "dev/expanded")) (make-directory "dev/expanded")))
(when dev (when (not (directory-exists? "dev/ast")) (make-directory "dev/ast")))

; TODO: delete this next line when we allow custom out files
(set! out (current-output-port))
(unless (output-port? out)
  (raise-user-error "no output specified"))

(unless (input-port? input)
  (raise-user-error "no input specified"))

(define basename (first (string-split (last (string-split in "/")) ".")))

(define EXPANDED (expand-file in))
(when dev (display-to-file EXPANDED
                           (string-append "dev/expanded/" basename "_exp.rkt")
                           #:exists 'replace))

(define AST (build-ast EXPANDED))
(when dev (pretty-display AST (open-output-file
                               (string-append "dev/ast/" basename "_ast.rkt")
                               #:exists 'replace)))

(define PASSED-AST (full-pass AST))
(when dev (pretty-display PASSED-AST (open-output-file
                                      (string-append "dev/ast/" basename "_passed_ast.rkt")
                                      #:exists 'replace)))

(define WAT (build-wat PASSED-AST))
(when (not (directory-exists? "out")) (make-directory "out"))
; Copy over the rasm js module and the example index for easy operating
(copy-file (string->path "./rasm.js") (string->path "./out/rasm.js") #t)
(copy-file (string->path "./example_index.js") (string->path "./out/index.js") #t)
(pretty-display WAT (open-output-file
                     (string-append "out/" basename ".wat")
                     #:exists 'replace))

(newline out)
(flush-output out)





