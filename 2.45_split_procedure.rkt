#lang racket

(require sicp-pict)

(define (split s1 s2)
  (lambda (painter n)
    (define (recurse k)
      (if (= k 0)
          painter
          (let ((smaller (recurse (- k 1))))
            (s1 painter (s2 smaller smaller)))))
    (recurse n)))

(define right-split (split beside below))

(define up-split (split below beside))
