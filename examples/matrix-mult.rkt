#lang racket/base
(require math
         feature-profile)

;; from an email on the racket ml by Berthold Bäuml on Sun 1/20/13

(define dim 200)

(define (random-matrix)
  (build-matrix dim dim (lambda (i j) (random))))

(feature-profile
 (void (matrix* (random-matrix) (random-matrix))))
