#lang racket

(require "structs.rkt" "profile-helpers.rkt" "utils.rkt"
         syntax/id-table
         ;; for contracts
         contract-profile
         ;; for function latent marks
         (for-template racket/base))

(provide default-features default-functional-latent-marks)

(define default-features
  (list
   (feature "Contracts" contract-continuation-mark-key
            ;; contract grouper
            (lambda (b)
              (define blame
                (if (pair? b)
                    (blame-add-missing-party (car b) (cdr b))
                    b))
              (format "~a ~a"
                      (blame-value blame)
                      (blame-contract blame))) ;; TODO srclocs too
            ;; contract-specific analysis
            (lambda (f-p)
              (define samples
                (feature-report-raw-samples f-p))
              (define custom-samples ; first layer of the core only, or #f if empty
                (for/list ([c-s (feature-report-core-samples f-p)])
                  (and (not (empty? c-s))
                       (first c-s))))
              ;; the analysis is in the contract-profile collect
              (analyze-contract-samples custom-samples samples)
              ;; also call basic analysis with custom grouping
              (print-feature-profile f-p)))
   (feature "Output" 'feature-profile:text-IO #f #f)
   (feature "Generic Sequences" 'feature-profile:generic-sequence #f #f)
   (feature "TR Casts" 'feature-profile:TR-dynamic-check #f #f)
   (feature "Keyword and Optional Arguments" 'feature-profile:kw-opt-protocol #f #f)
   (feature "Pattern Matching" 'feature-profile:pattern-matching #f #f)
   (feature "Send Dispatch" 'feature-profile:send-dispatch #f #f)
   ;; recommendation: use send-generic instead of send
   ))

(define default-functional-latent-marks
  (dict-set* (make-immutable-free-id-table)
             #'print       'feature-profile:text-IO
             #'printf      'feature-profile:text-IO
             #'fprintf     'feature-profile:text-IO
             #'display     'feature-profile:text-IO
             #'displayln   'feature-profile:text-IO
             #'newline     'feature-profile:text-IO
             #'write       'feature-profile:text-IO
             #'write-byte  'feature-profile:text-IO
             #'write-bytes 'feature-profile:text-IO
             ))
