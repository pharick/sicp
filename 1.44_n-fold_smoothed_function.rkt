#lang racket

(require sicp)
(require rackunit)

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (repeated f n)
  (define (iter n res)
    (if (= n 0)
        res
        (iter (- n 1) (compose f res))))
  (iter (- n 1) f))

(define (smooth-nth f n)
  (repeated smooth n))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(check-equal? (round ((smooth square) 3)) 9.0)
(check-equal? (round ((smooth cube) 10)) 1000.0)
