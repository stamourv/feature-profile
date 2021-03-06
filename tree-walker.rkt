#lang racket

;; tree-walker to turn latent marks into active marks

(require syntax/parse)
(require rackunit)

(provide make-latent-mark-compile-handler)


(define-namespace-anchor orig-namespace)

;; looks for syntax properties with key `key', and wraps the tagged expressions
;; with continuation marks
;; mostly inspired by the errortrace compile handler
(define (make-latent-mark-compile-handler keys functions-to-mark)
  (define orig  (current-compile))
  (define reg   (namespace-module-registry (current-namespace)))
  (define phase (namespace-base-phase (current-namespace)))
  (namespace-attach-module (namespace-anchor->namespace orig-namespace)
                           'racket/base)
  (namespace-attach-module (namespace-anchor->namespace orig-namespace)
                           'feature-profile/tree-walker)

  (lambda (e immediate-eval?)
    (orig
     ;; errortrace had some code involving namespace-module-registry and
     ;; namespace-base-phase, which I don't think I need.
     (let ([e2 (expand-syntax
                    (if (syntax? e)
                        e
                        (namespace-syntax-introduce
                         (datum->syntax #f e))))])
           (for/fold ([res e2])
               ([k keys])
             (code-walk res k functions-to-mark)))
     immediate-eval?)))

(define orig-inspector (variable-reference->module-declaration-inspector
                        (#%variable-reference)))
(define (rearm orig new)
  (syntax-rearm new orig))
(define (disarm orig)
  (syntax-disarm orig orig-inspector))


(define (add-props props-stx stx)
  ;; TODO does not get keys that are not interned symbols. could be a problem
  (define prop-keys (syntax-property-symbol-keys props-stx))
  (rearm props-stx
         (for/fold ([stx stx])
             ([k prop-keys])
           (syntax-property stx k (syntax-property props-stx k)))))


;; limitation: since we quote latent mark keys, can only really be symbols
;; limitation: does not traverse syntax time code (begin-for-syntax, etc.)
;;   this means that we don't profile metaprograms, or runtime code that
;;   comes from a begin-for-syntax and then is used for template (but no one
;;   does that)
;;   to fix: code-walk should take a phase arg, then use literal-sets at the
;;     right phase (o/w won't match properly), and insert w-c-ms (and requires
;;      to bind it) at the right phase
(define (code-walk stx key functions-to-mark)
  (define mark? (syntax-property stx key))
  ;; default payload, source location
  ;; make sure this doesn't allocate (we quote the vector, so we're ok)
  (define source (vector (syntax-source stx)
                         (syntax-line stx)
                         (syntax-column stx)
                         (syntax-position stx)
                         (syntax-span stx)))
  (define payload (and mark?
                       (if (eq? mark? #t)
                           source
                           ;; propagate custom payload
                           mark?)))
  (define-syntax-rule (mark/payload to-mark payload*)
    (add-props stx
               (quasisyntax/loc stx
                 (with-continuation-mark
                  '#,key '#,payload*
                  to-mark))))
  (define-syntax-rule (maybe-mark to-mark)
    (if mark?
        (mark/payload to-mark payload)
        (add-props stx (syntax/loc stx to-mark))))
  (define (recur x) (code-walk x key functions-to-mark))
  (define disarmed (disarm stx))
  (syntax-parse disarmed
    #:literal-sets (kernel-literals)

    ;; no latent mark to activate, but a function we want to watch
    [((~and app #%plain-app) x:id y ...)
     #:do [(define target-key (dict-ref functions-to-mark #'x #f))]
     #:when (and target-key (equal? target-key key))
     (with-syntax ([x* (recur #'x)]
                   [(y* ...) (for/list ([y (in-list (syntax->list #'(y ...)))])
                               (define res (recur y))
                               (syntax-parse y
                                 ;; TODO also needs to do the traversal right
                                 [(a . b) ; complex form, add an antimark
                                  #`(with-continuation-mark
                                     '#,key 'antimark
                                     #,res)]
                                 [_ ; simple form, no need for an antimark
                                  res]))])
       (mark/payload (app x* y* ...) source))]


    ;; some things we don't want to traverse at all
    [((~or #%require #%provide quote quote-syntax #%top #%variable-reference
           #%declare begin-for-syntax define-syntaxes)
      . x)
     stx]
    [(head . x) ; phase-shifted code
     #:when (and (identifier? #'head) (not (identifier-binding #'head)))
     stx]

    ;; some things we traverse all of
    [((~and head (~or #%expression begin begin0 #%plain-app if
                      with-continuation-mark))
      x ...)
     (with-syntax ([(x* ...) (map recur (syntax->list #'(x ...)))])
       (maybe-mark (head x* ...)))]

    ;; some things we traverse selectively
    [((~and head set!) var val)
     (with-syntax ([val* (recur #'val)])
       (maybe-mark (head var val*)))]
    [((~and head (~or #%plain-lambda define-values))
      formals body ...)
     (with-syntax ([(body* ...) (map recur (syntax->list #'(body ...)))])
       (maybe-mark (head formals body* ...)))]
    [((~and head case-lambda) [formals body ...] ...)
     (with-syntax ([((body* ...) ...)
                    (for/list ([clause-body (syntax->list #'((body ...) ...))])
                      (map recur (syntax->list clause-body)))])
       (maybe-mark (head [formals body* ...] ...)))]
    [((~and head (~or let-values letrec-values)) ([lhs rhs] ...) body ...)
     (with-syntax ([(rhs*  ...) (map recur (syntax->list #'(rhs  ...)))]
                   [(body* ...) (map recur (syntax->list #'(body ...)))])
       (maybe-mark (head ([lhs rhs*] ...) body* ...)))]
    [((~and head letrec-syntaxes+values) stx-bindings ([lhs rhs] ...) body ...)
     (with-syntax ([(rhs*  ...) (map recur (syntax->list #'(rhs  ...)))]
                   [(body* ...) (map recur (syntax->list #'(body ...)))])
       (maybe-mark (head stx-bindings ([lhs rhs*] ...) body* ...)))]
    [((~and head (~or module
                      module*
                      ;; sometimes (e.g. across a `(begin-for-syntax (dynamic-require) ...)'
                      ;; or when TR requires something), the `module' identifier
                      ;; at the beginning of a module is at phase 1. None of the
                      ;; other identifiers are, tough. No idea what's going on,
                      ;; but that solves it.
                      (~literal module #:phase (namespace-base-phase))
                      (~literal module* #:phase (namespace-base-phase))))
      name lang m-b-form)
     (syntax-parse (disarm #'m-b-form)
       [(m-b body ...)
        (with-syntax ([(body* ...) (map recur (syntax->list #'(body ...)))])
          (maybe-mark (head name lang (m-b body* ...))))])]
    [x:identifier
     (maybe-mark x)]
    [_
     (error "non-exhaustive pattern match" (syntax->datum stx))]))

(module+ test
  (define-syntax-rule (syntax-check-equal? a b)
    (check-equal?
     (syntax->datum a)
     (syntax->datum b)))

  (syntax-check-equal? (code-walk #'a 'x (hash)) #'a)
  (syntax-check-equal? (code-walk (syntax-property #'a 'b 'c) 'c (hash)) #'a)
  (syntax-check-equal?
   (code-walk (syntax-property #'a 'b 'c) 'b (hash))
   #'(with-continuation-mark 'b 'c a))
  (syntax-check-equal?
   (code-walk (expand #'(+ '1 '2)) 'x (hash))
   #'(#%app + '1 '2))
  (syntax-check-equal?
   (code-walk (expand #`(+ 1 #,(syntax-property #'2 'a 'b))) 'b (hash))
   #'(#%app + '1 '2))
  (syntax-check-equal?
   (code-walk (expand #`(+ 1 #,(syntax-property #'(+ 2 3) 'a 'b))) 'a (hash))
   #'(#%app + '1 (with-continuation-mark 'a 'b (#%app + '2 '3))))
  (syntax-check-equal?
   (code-walk (expand #`(λ (x) #,(syntax-property #'(x) 'a 'b))) 'a (hash))
   #'(#%expression (lambda (x) (with-continuation-mark 'a 'b (#%app x)))))
  (syntax-check-equal?
   (code-walk (expand #`(let ([x #,(syntax-property #'(+ 3 2) 'b 'c)])
                          (λ (y z)
                            #,(syntax-property
                               #'(+ x y z)
                               'a 'c))))
              'a (hash))
   #'(let-values (((x) (#%app + '3 '2)))
       (lambda (y z)
         (with-continuation-mark 'a 'c
           (#%app + x y z)))))
  (syntax-check-equal?
   (code-walk (expand (syntax-property
                       #`(letrec ([f (λ (x) #,(syntax-property
                                               #'(if (= x 0)
                                                     1
                                                    (* x (f (- x 1))))
                                               'b 'c))])
                           (f 5))
                       'b 'c))
              'b (hash))
   #'(with-continuation-mark 'b 'c
       (letrec-values (((f) (lambda (x)
                              (with-continuation-mark 'b 'c
                                (if (#%app = x '0)
                                    '1
                                    (#%app * x (#%app f (#%app - x '1))))))))
         (#%app f '5))))
  (syntax-check-equal?
   (code-walk (expand #`(let ()
                          (define x 3)
                          #,(syntax-property #'(set! x (+ 5 3)) 'a 'b)
                          (- 8 x)))
              'a (hash))
   #'(let-values ()
       (let-values (((x) '3))
         (with-continuation-mark 'a 'b (set! x (#%app + '5 '3)))
         (#%app - '8 x))))
  (syntax-check-equal?
   (code-walk (expand (syntax-property
                       #`((case-lambda
                           [() 3]
                           [(x) #,(syntax-property #'x 'a 'b)]
                           [(x y) (+ x y)])
                          3 2)
                       'a 'b))
              'a (hash))
   #'(with-continuation-mark 'a 'b
       (#%app (case-lambda (() '3)
                           ((x) (with-continuation-mark 'a 'b x))
                           ((x y) (#%app + x y)))
              '3 '2)))
  (syntax-check-equal?
   (code-walk (expand #`(module mymod racket
                          #,(syntax-property #'(+ 1 2) 'a 'b)))
              'a (hash))
   #'(module mymod racket
       (#%module-begin
        (module configure-runtime '#%kernel
          (#%module-begin (#%require racket/runtime-config) (#%app configure '#f)))
        (#%app call-with-values (lambda () (with-continuation-mark 'a 'b
                                             (#%app + '1 '2))) print-values))))

  (check-equal?
   (with-handlers ([exn:fail? (λ (exn) (void))])
     (code-walk #'(+ 1 2) 'a (hash)))
    (void))
   )
