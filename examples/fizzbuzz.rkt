#lang racket

;; original from: https://github.com/Rosetta-FizzBuzz/racket-FizzBuzz/blob/master/fb.rkt

(require feature-profile)

(define (divisible x n)
  (= 0 (modulo x n)))

(define (fizzbuzz n)
  (for ([i (range n)])
    (cond [(divisible i 15) (printf "FizzBuzz\n")]
          [(divisible i 5)  (printf "Buzz\n")]
          [(divisible i 3)  (printf "Fizz\n")]
          [else             (printf "~a\n" i)])))


(feature-profile-thunk
 (lambda ()
   (void
    (with-output-to-string
      (lambda ()
        (fizzbuzz 10000000))))))
