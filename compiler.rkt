#lang racket

(require "parser.rkt")
(require "ast.rkt")
(require "wat.rkt")

; Is it worth using typed racket??

;; TODO: Things of interest
; - handling lambdas, basically make every lambda a top level func???
; -- Maybe take the top level at first and then try to optimize later (with loops)
; - data types, just hardcode everything to f64 for now bc that's a safe bet
; - Provide/exports, we need to actually parse the spec
 
(define file-to-compile
  (command-line
   #:program "rasm"
   #:args (filename)
   filename))

(when (not (string? file-to-compile)) (error 'rasm "Expected filename"))

(define AST (build-ast (expand-file file-to-compile)))
(define WAT (build-wat AST))

(define basename (first (string-split (last (string-split file-to-compile "/")) ".")))
(display-to-file WAT
                 (string-append "out/" basename ".wat")
                 #:exists 'replace)
