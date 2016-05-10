#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "main.rkt")

;; raco feature-profile
;; profile the main submodule (if there is one), or the top-level module

(define file
  (command-line #:program (short-program+command-name)
                #:args (filename)
                filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(current-compile feature-profile-compile-handler)

(feature-profile
 (dynamic-require (module-to-profile file) #f))

(module test racket/base) ; don't run for testing
