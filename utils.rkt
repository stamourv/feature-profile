#lang racket/base

(provide (all-defined-out))

(define (make-interner)
  (define entry-table (make-hash))
  (define (intern-entry entry)
    (define key (car entry))
    (or (hash-ref entry-table key #f)
        (begin (hash-set! entry-table key entry) entry)))
  intern-entry)
