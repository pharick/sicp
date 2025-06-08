#lang racket

(require sicp)
(require rackunit)

(define (iterative-improve close-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  try)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess next)
      (< (abs (- (square next) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve
    (lambda (a b)
      (< (abs (- a b)) tolerance))
    (lambda (guess)
      (f guess)))
   first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (cube-root x)
  (define dx 0.00001)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) dx))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  ((iterative-improve close-enough? improve) 1.0))

(check-equal? (round (cube-root 27)) 3.0)
(check-equal? (round (cube-root 64)) 4.0)
