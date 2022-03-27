#lang racket

(provide expand-file)

(define (expand-file filename)
  (let ((file (open-input-file filename)))
    (define expansion (parameterize ([current-namespace (make-base-namespace)])
                        (expand
                         (read-syntax (object-name file) file))))
    (close-input-port file)
    expansion))