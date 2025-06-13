#lang racket

(require sicp)
(require rackunit)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

#|
(z (lambda (p q) p))

((lambda (m) (m x y)) (lambda (p q) p))

((lambda (p q) p) x y)

x
|#

(define (cdr z)
  (z (lambda (p q) q)))

#|
(z (lambda (p q) q))

((lambda (m) (m x y)) (lambda (p q) q))

((lambda (p q) q) x y)

y
|#

(define example (cons 1 2))

(check-equal? (car example) 1)
(check-equal? (cdr example) 2)
(check-equal? (cdr (cons 5 15)) 15)
