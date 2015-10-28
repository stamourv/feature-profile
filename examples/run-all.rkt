#lang racket

(require racket/runtime-path)

(define-runtime-path here ".")

(define examples
  '("composition.rkt"
    "fizzbuzz.rkt"
    "matrix-mult.rkt"
    "word-wrap.rkt"))

(for ([f examples])
  (system (format "~a -l feature-profile -t ~a"
                  (path->string (find-executable-path "racket"))
                  (build-path here f))))
