#lang racket

(require sicp)
(require rackunit)

(define (max a b c)
  (cond ((and (>= a b) (>= a c)) a)
        ((and (>= b a) (>= b c)) b)
        (else c)))

(define (secondToMax a b c)
  (cond ((or (and (>= a b) (<= a c)) (and (>= a c) (<= a b))) a)
        ((or (and (>= b a) (<= b c)) (and (>= b c) (<= b a))) b)
        (else c)))

(define (square x) (* x x))

(define (solution a b c)
  (+ (square (max a b c)) (square (secondToMax a b c))))

(check-equal? (solution 1 2 3) 13)
(check-equal? (solution 4 2 3) 25)
(check-equal? (solution 0 0 0) 0)
(check-equal? (solution 1 0 1) 2)
(check-equal? (solution 2 3 2) 13)
