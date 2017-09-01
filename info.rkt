#lang info

(define collection "feature-profile")
(define compile-omit-paths '("examples" "tests"))

(define deps '(("base" #:version "6.3")
               "contract-profile"
               "profile-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib"))

(define scribblings '(("scribblings/feature-profile.scrbl" () ("Performance Tools"))))

(define pkg-desc "Profiling language/library feature costs.")

(define version "1.0")

(define pkg-authors '(stamourv))

(define raco-commands
  '(("feature-profile"
     feature-profile/raco
     "profile overhead from various linguistic constructs"
     #f)))
