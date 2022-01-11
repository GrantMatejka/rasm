#lang typed/racket

;; TODO: this is just scratch paper/experimentation/not working

(command-line
   #:program "compiler"
   #:args (filename) ; expect one arg, the filename to compile
   filename)


(define file-to-compile
  (match (parse-command-line "compile" (current-command-line-arguments)
                             '()
                             (lambda (flag-accum file) file)
                             '("filename"))
    [(? string? s) s]
    [other 'Args "Expected single filename"]))

(define output-file (string-append file-to-compile ".wat"))
(define input-file (string-append file-to-compile ".rkt"))


(define (write-file [expr : Sexp]) : Void
  (define contents `(module ,expr))
  (display-to-file contents output-file))

(define (open-file) : String
  (port->string (open-input-file input-file) #:close? #t))

(expand '(+ 1 2))
