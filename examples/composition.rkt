#lang typed/racket

(require/typed feature-profile
               [feature-profile-thunk (-> (-> Any) Any)])

(define string-list
  (map number->string (range 1000000)))

(define (run)
  (for ([s string-list])
    (define n (cast (string->number s) Integer))
    (if (even? n)
        (printf "~a is even\n" n)
        (printf "~a is odd\n" n))))


(feature-profile-thunk
 (lambda ()
   (void
    (with-output-to-string
      (lambda ()
        (run))))))

;; TODO downside of using TR is that we don't see generic sequence costs
