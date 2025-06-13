#lang racket

(require sicp)
(require rackunit)

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (div-iter p d count)
  (if (= (remainder p d) 0)
      (div-iter (/ p d) d (+ count 1))
      count))

(define (car p)
  (div-iter p 2 0))

(define (cdr p)
  (div-iter p 3 0))

(define (power x n)
  (define (iter x n res)
    (if (= n 0)
        res
        (iter x (- n 1) (* res x))))
  (iter x n 1))

(define a 2)
(define b 3)
(define x (cons a b))
(define x2 (cons 0 1))
(define x3 (cons 1 0))

(check-equal? x 108)
(check-equal? x2 3)
(check-equal? x3 2)
(check-equal? (car x) a)
(check-equal? (cdr x) b)
(check-equal? (car x2) 0)
(check-equal? (cdr x2) 1)
(check-equal? (car x3) 1)
(check-equal? (cdr x3) 0)
