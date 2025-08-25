#lang racket

(require rackunit)

(define (make-accumulator sum)
  (define (accumulator n)
    (begin (set! sum (+ sum n))
           sum))
  accumulator)

(define A (make-accumulator 5))

(check-equal? (A 10) 15)
(check-equal? (A 10) 25)
(check-equal? (A -5) 20)
(check-equal? ((make-accumulator 0) 5) 5)
