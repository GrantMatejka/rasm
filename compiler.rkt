#lang racket

(require racket/pretty
         "expand.rkt"
         "passes.rkt"
         "wat.rkt")

(pretty-print-columns 100)

(define in #f)
(define dev #f)

(command-line
 #:once-any
 [("--dev") "write intermediate forms to dev files. Expects \"dev/expanded\" and \"dev/ast\" to exist"
            (set! dev #t)]
 #:args ([source #f])
 (set! in source))

(define input (open-input-file in))

(when dev (when (not (directory-exists? "dev")) (make-directory "dev")))
(when dev (when (not (directory-exists? "dev/expanded")) (make-directory "dev/expanded")))
(when dev (when (not (directory-exists? "dev/ast")) (make-directory "dev/ast")))

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




