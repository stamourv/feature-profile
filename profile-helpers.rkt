#lang racket

;; "framework" to make building custom profilers easier

(require profile/sampler profile/utils
         unstable/list
         racket/contract/private/blame
         (for-syntax syntax/parse)
         "structs.rkt")

(provide custom-profile
         print-feature-profile)

;; TODO add kw args to pass to make-sampler
(define-syntax (custom-profile stx)
  (syntax-parse stx
    [(_ features* q? body ...)
     #'(let* ([features features*]
              [sampler  (create-sampler (current-thread)
                                        0.005
                                        (current-custodian)
                                        (map feature-key features))])
         (begin0 (begin body ...)
           (unless q?
           (let ()
             (sampler 'stop)
             (define samples (sampler 'get-snapshots))
             (define custom-samples (sampler 'get-custom-snapshots))
             (printf "~a samples\n" (length custom-samples))

             ;; do basic analysis no matter what
             (define feature-reports
               (for/list ([f (in-list features)] [i (in-naturals)])
                 (define feature-core-samples
                   (for/list ([s (in-list custom-samples)])
                     ;; extract the relevant part of the core sample
                     ;; and only when there is a mark of the kind we're looking
                     ;; for (o/w payload looks like #f, it's the none-v from
                     ;; continuation-mark-set->list*.)
                     ;; this rules out payloads of #f. oh well
                     (for/list ([mark (in-list s)]
                                #:when (vector-ref mark i))
                       (vector-ref mark i))))
                 (analyze-custom-samples f samples feature-core-samples)))
             (for ([f-p (in-list
                         (sort feature-reports >
                               #:key feature-report-feature-time))]
                   #:when (> (feature-report-feature-time f-p) 0))
               (define feature  (feature-report-feature f-p))
               (define analysis (feature-analysis feature))
               (if analysis
                   ;; custom analysis
                   (analysis f-p)
                   ;; basic analysis
                   (print-feature-profile f-p)))))))]))


(define (samples-time samples)
  (for/sum ([s (in-list samples)])
    (cadr s)))

;; original srcloc->string does not support source location vectors
(require (prefix-in r: racket))
(define (srcloc->string s)
  (r:srcloc->string (apply make-srcloc (vector->list s))))

(define (get-grouper feature) ; default is srcloc->string
  (or (feature-grouper feature) srcloc->string))


;; pre-processing, before passing on to actual analysis (basic or custom)
(define (analyze-custom-samples feature samples* custom-core-samples)
  ;; car of samples* is total time, car of each sample is thread id
  ;; for now, we just assume a single thread. fix this eventually.
  (define total-time (car samples*))
  ;; reverse is there to sort samples in forward time, which get-times
  ;; needs.
  (define samples    (get-times (map cdr (reverse (cdr samples*)))))
  (define live-samples
    (for/list ([s   (in-list samples)]
               [c-s (in-list custom-core-samples)]
               ;; there is a mark
               #:when (not (empty? c-s))
               ;; top is not an antimark (otherwise we don't want to count)
               #:when (not (eq? (car c-s) 'antimark)))
      ;; basic analysis uses only the top mark
      (cons (first c-s) s)))

  (define feature-time     (samples-time live-samples))

  (define grouper (get-grouper feature))
  (define grouped
    (sort (group-by (lambda (x) (grouper (car x)))
                    live-samples)
          > #:key samples-time #:cache-keys? #t))

  (feature-report
   feature custom-core-samples samples* grouped total-time feature-time))

;; basic analysis. group by feature instance (payload)
;; grouper takes feature instances to representatives of an equivalence class
(define (print-feature-profile f-p)
  (match-define
      (feature-report
       feature core-samples raw-samples grouped total-time feature-time)
    f-p)

  (define feature-time-ratio (/ feature-time
                                (max total-time 1)
                                1.0))

  (newline) (newline)
  (printf "~a\n" (feature-name feature))
  (printf "account(s) for ~a% of total running time\n"
          (~r (* 100 feature-time-ratio) #:precision 2))
  (printf "~a / ~a ms\n\n" feature-time total-time)

  (printf "Cost Breakdown\n")
  (define grouper (get-grouper feature))
  (for ([g grouped])
    ;; TODO use path shortening
    (define representative (grouper (car (first g))))
    (unless (eq? representative 'antimark)
      (printf "  ~a ms : ~a\n" (samples-time g) representative))))
