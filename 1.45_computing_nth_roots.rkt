#lang racket

(require sicp)
(require rackunit)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (repeated f n)
  (define (iter n res)
    (if (= n 0)
        res
        (iter (- n 1) (compose f res))))
  (iter (- n 1) f))

(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeated average-damp (floor (log n 2)))
                  (lambda (y)
                    (/ x (power y (- n 1)))))
                 1.0)))

(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (root3 x) ((nth-root 3) x))

(define (root4 x) ((nth-root 4) x))

(check-equal? (round (root3 27)) 3.0)
(check-equal? (round (root3 64)) 4.0)
(check-equal? (round (root4 81)) 3.0)
(check-equal? (round (root4 10000)) 10.0)
