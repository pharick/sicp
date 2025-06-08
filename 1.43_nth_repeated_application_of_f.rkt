#lang racket

(require sicp)
(require rackunit)

(define (repeated f n)
  (define (iter n res)
    (if (= n 0)
        res
        (iter (- n 1) (compose f res))))
  (iter (- n 1) f))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(check-equal? ((repeated square 1) 6) 36)
(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated inc 10) 10) 20)
