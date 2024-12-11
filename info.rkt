#lang info.rkt

(define collection "rasm")
(define deps '("base" "typed-racket-lib"))
(define license '(MIT))
(define racket-launcher-libraries '("compiler.rkt"))
(define racket-launcher-names '("rasm-compile"))
(define raco-commands '(("rasm-compile" rasm/compiler "A Racket to WASM compiler." #f)))
