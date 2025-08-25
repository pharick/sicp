#lang racket

(require rackunit)

(define (make-f)
  (let ([first? #t])                 ; mutable state, shared across calls
    (lambda (x)
      (if first?
          (begin (set! first? #f) x) ; first call returns its argument
          0))))                      ; later calls return 0

;; -- tests --

(define f (make-f))

(define left f)

(check-equal? (+ (left 0) (left 1)) 0)

(define right (make-f))

(check-equal? (+ (right 1) (right 0)) 1)
