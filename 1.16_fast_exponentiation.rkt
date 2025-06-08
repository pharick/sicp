#lang racket

(require sicp)
(require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (solution b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (* b b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

(check-equal? (solution 10 0) 1)
(check-equal? (solution 3 20) (expt 3 20))
(check-equal? (solution 2 10) (expt 2 10))
(check-equal? (solution 0 5) 0)
