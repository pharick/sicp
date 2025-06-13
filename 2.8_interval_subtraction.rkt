#lang racket

(require sicp)
(require rackunit)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define lower1 5)

(define lower2 15)

(define upper1 10)

(define upper2 25)

(define interval1 (make-interval lower1 upper1))

(define interval2 (make-interval lower2 upper2))

(check-equal? (sub-interval interval1 interval2) '(-20 . -5))
(check-equal? (sub-interval interval2 interval1) '(5 . 20))
(check-equal? (sub-interval (make-interval (- 3) (- 3)) (make-interval 1 2)) '(-5 . -4))
