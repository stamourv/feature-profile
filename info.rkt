#lang info

(define collection "feature-profile")
(define compile-omit-paths '("case-studies" "overhead" "examples" "tests"))

(define deps '("base" "contract-profile" "unstable-list-lib" "profile-lib"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib"))

(define scribblings '(("scribblings/feature-profile.scrbl" () (tool-library))))

(define pkg-desc "Profiling language/library feature costs.")

(define version "1.0")

(define pkg-authors '(stamourv))
