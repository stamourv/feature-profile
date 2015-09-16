#lang racket

(require compiler/compile-file compiler/cm
         racket/runtime-path
         "structs.rkt" "tree-walker.rkt" "features.rkt" "profile-helpers.rkt"
         (for-syntax syntax/parse))

(provide feature-profile-compile-handler
         compile-file/feature-profile managed-compile-zo/feature-profile
         (rename-out [feature-profile/user feature-profile])
         feature-profile-thunk
         ;; for custom compile-handlers
         make-latent-mark-compile-handler
         default-syntactic-latent-mark-keys
         default-functional-latent-marks
         default-features)

(define default-syntactic-latent-mark-keys
  (map feature-key default-features))

(define feature-profile-compile-handler
  (make-latent-mark-compile-handler default-syntactic-latent-mark-keys
                                    default-functional-latent-marks))

(define ((make-entry-point do-compile) file)
  (define old-ns (current-namespace))
  (parameterize ([current-compile feature-profile-compile-handler]
                 [read-accept-reader #t]
                 [current-namespace (make-base-namespace)])
    (namespace-attach-module old-ns ;; TODO should that be in the tree walker?
                             'feature-profile/tree-walker
                             (current-namespace))
    (do-compile file)))

(define compile-file/feature-profile       (make-entry-point compile-file))
(define managed-compile-zo/feature-profile (make-entry-point managed-compile-zo))

(current-compile feature-profile-compile-handler)


(define-syntax (feature-profile/user stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:features e-fs) #:defaults ([e-fs #'default-features]))
             (~optional (~seq #:quiet? q?) #:defaults ([q? #'#f])))
        ...
        body ...)
     #'(custom-profile e-fs q? body ...)]))

(define (feature-profile-thunk thunk #:features [e-fs default-features] #:quiet? [q? #f])
  (feature-profile/user #:features e-fs #:quiet? q? (thunk)))
