#lang racket

(require "parser.rkt")
(require "ast.rkt")
(require "wat.rkt")
(require racket/pretty)

; Is it worth using typed racket??

;; TODO: Things of interest
; - handling lambdas, basically make every lambda a top level func???
; -- Maybe take the top level at first and then try to optimize later (with loops)
; - data types, just hardcode everything to f64 for now bc that's a safe bet
; - Provide/exports, we need to actually parse the spec

; Do we need to have a unique identifier for every var?
;   Don't know since wasm has func scoped vars, but I tink easy enough to just do to be safe
; RacketScript uses syntax-parse rather than kernel-syntax-case????

; Things of interest:
;  How are we handling types? var & return?
;  Make every lambda top level? Where we pass in all vars we're interested in from env?
;  Mmultiple return values?

(define file-to-compile
  (command-line
   #:program "rasm"
   #:args (filename)
   filename))

(when (not (string? file-to-compile)) (error 'rasm "Expected filename"))

(define basename (first (string-split (last (string-split file-to-compile "/")) ".")))

(define EXPANDED (expand-file file-to-compile))
(display-to-file EXPANDED
                 (string-append "dev/expanded/" basename "_exp.rkt")
                 #:exists 'replace)

(define AST (build-ast EXPANDED))
(display-to-file AST
                 (string-append "dev/ast/" basename "_ast.rkt")
                 #:exists 'replace)

(define WAT (build-wat AST))
(display-to-file WAT
                 (string-append "out/" basename ".wat")
                 #:exists 'replace)
