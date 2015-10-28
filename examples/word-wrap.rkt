#lang at-exp racket

;; from http://rosettacode.org/wiki/Word_wrap#Racket

(require scribble/text/wrap)
(define text
  @(λ xs (regexp-replace* #rx" *\n *" (string-append* xs) " ")){
    In olden times when wishing still helped one, there lived a king whose
    daughters were all beautiful, but the youngest was so beautiful that the
    sun itself, which has seen so much, was astonished whenever it shone in her
    face.  Close by the king's castle lay a great dark forest, and under an old
    lime-tree in the forest was a well, and when the day was very warm, the
    king's child went out into the forest and sat down by the side of the cool
    fountain, and when she was bored she took a golden ball, and threw it up on
    high and caught it, and this ball was her favorite plaything.})
;; (for-each displayln (wrap-line text 60))

(define (wrap-old words width)
  (define (maybe-cons xs xss)
    (if (empty? xs) xss (cons xs xss)))
  (match/values
    (for/fold ([lines '()] [line '()] [left width]) ([w words])
      (define n (string-length w))
      (cond
        [(> n width) ; word longer than line => line on its own
         (values (cons (list w) (maybe-cons line lines)) '() width)]
        [(> n left)  ; not enough space left => new line
         (values (cons line lines) (list w) (- width n 1))]
        [else
         (values lines (cons w line) (- left n 1))]))
    [(lines line _)
     (for ([line (reverse (cons line lines))])
       (displayln line))]))



(define (wrap words width)
  (for/fold ([left width]) ([w words])
    (define n (string-length w))
    (cond
     [(> n width) ; word longer than line => line on its own
      (printf "\n~a\n" w)
      width]
     [(> n left)  ; not enough space left => new line
      (printf "\n~a " w)
      (- width n 1)]
     [else
      (printf "~a " w)
      (- left n 1)]))
  (newline))


(require feature-profile)

(feature-profile-thunk
 (lambda ()
   (void
    (with-output-to-string
      (lambda ()
        (for ([x (in-range 10000)]) (wrap (string-split text) 70)))))))
