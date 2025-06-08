#lang racket

(require sicp)
(require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (double a) (* a 2))

(define (halve a) (/ a 2))

(define (mul-iter a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ c a)))))
  (iter a b 0))

(check-equal? (mul-iter 1 1) 1)
(check-equal? (mul-iter 40 30) (* 40 30))
(check-equal? (mul-iter 5 0) 0)
(check-equal? (mul-iter 5 15) (* 5 15))
