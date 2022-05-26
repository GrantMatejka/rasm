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

(unless (input-port? input)
  (raise-user-error "no input specified"))

(define basename (first (string-split (last (string-split in "/")) ".")))

(define output (open-output-file
                (string-append "out/" basename ".wat")
                #:exists 'replace))

(define EXPANDED (expand-file in))

(define AST (build-ast EXPANDED))

(define PASSED-AST (full-pass AST))

(define WAT (build-wat PASSED-AST))

; When we want to save dev records
(when dev
  (when (not (directory-exists? "dev")) (make-directory "dev"))
  (when (not (directory-exists? "dev/expanded")) (make-directory "dev/expanded"))
  (when (not (directory-exists? "dev/ast")) (make-directory "dev/ast"))
  (define output-exp (open-output-file
                      (string-append "dev/expanded/" basename "_exp.rkt")
                      #:exists 'replace))
  (define output-ast (open-output-file
                      (string-append "dev/ast/" basename "_ast.rkt")
                      #:exists 'replace))
  (define output-past (open-output-file
                       (string-append "dev/ast/" basename "_passed_ast.rkt")
                       #:exists 'replace))
  (pretty-display EXPANDED output-exp)
  (pretty-display AST output-ast)
  (pretty-display PASSED-AST output-past)
  (close-output-port output-exp)
  (close-output-port output-ast)
  (close-output-port output-past))

(when (not (directory-exists? "out")) (make-directory "out"))
; Copy over the rasm js module and the example index for easy operating
(copy-file (string->path "./rasm.js") (string->path "./out/rasm.js") #t)
(copy-file (string->path "./example_index.js") (string->path "./out/index.js") #t)
(pretty-display WAT output)

(close-input-port input)
(close-output-port output)



