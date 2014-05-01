#lang racket

(provide (all-defined-out))

(struct feature (name key grouper analysis))

(struct feature-report
  (feature core-samples raw-samples grouped-samples total-time feature-time))
