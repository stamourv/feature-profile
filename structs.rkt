#lang racket

(provide (all-defined-out))

(struct feature (name key grouper analysis)
        #:methods gen:custom-write
        [(define (write-proc f port mode)
           (write-string (format "#<feature: ~s>" (feature-name f))
                         port))])

(struct feature-report
  (feature core-samples raw-samples grouped-samples total-time feature-time))
