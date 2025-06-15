#lang racket

(require sicp)
(require rackunit)

(define (same-parity x . xs)
  (filter (if (even? x) even? odd?) (cons x xs)))

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (= (remainder x 2) 1))

(define (filter pred lst)
  (define (iter lst res)
    (cond ((null? lst) (reverse res))
          ((pred (car lst)) (iter (cdr lst) (cons (car lst) res)))
          (else (iter (cdr lst) res))))
  (iter lst nil))

(define (reverse list)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (cons (car lst) acc))))
  (iter list nil))

(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6))
(check-equal? (same-parity 1) '(1))
