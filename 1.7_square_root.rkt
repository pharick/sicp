#lang racket

(require sicp)
(require rackunit)

(define (sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (< (abs (- guess new-guess)) 0.0001)
      new-guess
      (sqrt-iter new-guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (square-root x)
  (sqrt-iter 1.0 x))

(check-equal? (round (* 1000 (square-root 4.0))) 2000.0)
(check-equal? (round (* 1000 (square-root 100.0))) 10000.0)
(check-equal? (round (* 1000 (square-root 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (square-root 0.04))) 200.0)
(check-equal? (round (* 1000 (square-root 10000000000000000.0))) 100000000000.0)
