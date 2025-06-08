#lang racket

(require sicp)
(require rackunit)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(check-equal? (round (* 100 (fixed-point cos 1.0))) 74.0)
(newline)

(check-equal? (round (* 100 (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))) 126.0)
(newline)

(define (f x)
  (/ (log 1000) (log x)))

(define (f-avg x)
  (average x
           (/ (log 1000) (log x))))

(define (average x y)
  (/ (+ x y) 2))
