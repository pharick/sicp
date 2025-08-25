#lang racket

(require rackunit)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (sicp-random n)
  (if (and (exact? n) (integer? n))
      (random n)
      (* n (random))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (sicp-random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y1)))
      (and
       (>= x x1)
       (<= x x2)
       (>= y y1)
       (<= y y2))))
  (let ((fract (monte-carlo trials experiment)))
    (* (- x2 x1)
       (- y2 y1)
       fract)))

;; -- tests --

(define (square x) (* x x))

(define (square-predicate? x y)
  (< (+ (square x)
        (square y))
     1.0))

(define (test-pi trials)
  (exact->inexact
   (estimate-integral square-predicate? -1.0 1.0 -1.0 1.0 trials)))


(define attempt (floor (* 10 (test-pi 10000))))

(check-true (or
             (>= attempt 30.0)
             (<= attempt 32.0)))
